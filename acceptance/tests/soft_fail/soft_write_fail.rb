test_name "soft write failure" do
  on master, "puppet master --configprint confdir"
  confdir = stdout.chomp
  puppetdb_conf = "#{confdir}/puppetdb.conf"

  step "backup puppetdb.conf and prepare with new setting" do
    on master, "rm -f #{puppetdb_conf}.soft_write_fail"
    on master, "cp -p #{puppetdb_conf} #{puppetdb_conf}.soft_write_fail"
    on master, "echo 'soft_write_failure = true' >> #{puppetdb_conf}"
  end

  teardown do
    # Restore original puppetdb.conf
    on master, "rm -f #{puppetdb_conf}"
    on master, "mv #{puppetdb_conf}.soft_write_fail #{puppetdb_conf}"
  end

  names = hosts.map(&:name)
  tmpdir = master.tmpdir('storeconfigs')

  manifest_file_export = manifest_file_collect = nil

  step "prepare sample content" do
    manifest_export = names.map do |name|
      <<-PIECE
node "#{name}" {
  @@notify { "Hello from #{name}": }
  notify { "#{name} only": }
}
      PIECE
    end.join("\n")

    manifest_file_export = File.join(tmpdir, 'site-export.pp')
    create_remote_file(master, manifest_file_export, manifest_export)

    manifest_collect = names.map do |name|
      <<-PIECE
node "#{name}" {
  @@notify { "Hello from #{name}": }
  notify { "#{name} only": }
  Notify <<||>>
}
      PIECE
    end.join("\n")

    manifest_file_collect = File.join(tmpdir, 'site-collect.pp')
    create_remote_file(master, manifest_file_collect, manifest_collect)

    on master, "chmod -R +rX #{tmpdir}"
  end

  with_puppet_running_on master, {
    'master' => {
      'autosign' => 'true',
      'manifest' => manifest_file_export,
    }} do

    step "Run agent with no collection and puppetdb stopped making sure it completes" do
      stop_puppetdb(database)

      run_agent_on hosts, "--test --server #{master}"
      assert_match(/Failed to submit 'replace facts'/, stdout)
      assert_match(/Failed to submit 'replace catalog'/, stdout)

      start_puppetdb(database)
    end
  end

  with_puppet_running_on master, {
    'master' => {
      'autosign' => 'true',
      'manifest' => manifest_file_collect,
    }} do

    step "Run agent with collection and puppetdb stopped making sure it fails" do
      stop_puppetdb(database)

      run_agent_on hosts, "--test --server #{master}", :acceptable_exit_codes => 1

      start_puppetdb(database)
    end
  end
end
