require 'cgi'
require 'beaker/dsl/install_utils'
require 'beaker/dsl/helpers'
require 'pp'
require 'set'
require 'test/unit/assertions'
require 'json'
require 'inifile'

module PuppetDBExtensions
  include Test::Unit::Assertions

  GitReposDir = Beaker::DSL::InstallUtils::SourcePath

  LeinCommandPrefix = "cd #{GitReposDir}/puppetdb; LEIN_ROOT=true LEIN_SNAPSHOTS_IN_RELEASE=true"

  def self.initialize_test_config(options, os_families)

    base_dir = File.join(File.dirname(__FILE__), '..')

    install_type = get_option_value(options[:puppetdb_install_type],
      [:git, :package, :pe], "install type", "PUPPETDB_INSTALL_TYPE", :git)

    install_mode =
        get_option_value(options[:puppetdb_install_mode],
                         [:install, :upgrade], "install mode",
                         "PUPPETDB_INSTALL_MODE", :install)

    database =
        get_option_value(options[:puppetdb_database],
            [:postgres, :embedded], "database", "PUPPETDB_DATABASE", :postgres)

    validate_package_version =
        get_option_value(options[:puppetdb_validate_package_version],
            [:true, :false], "'validate package version'",
            "PUPPETDB_VALIDATE_PACKAGE_VERSION", :true)

    expected_rpm_version =
        get_option_value(options[:puppetdb_expected_rpm_version],
            nil, "'expected RPM package version'",
            "PUPPETDB_EXPECTED_RPM_VERSION", nil)

    expected_deb_version =
        get_option_value(options[:puppetdb_expected_deb_version],
                         nil, "'expected DEB package version'",
                         "PUPPETDB_EXPECTED_DEB_VERSION", nil)

    use_proxies =
        get_option_value(options[:puppetdb_use_proxies],
          [:true, :false], "'use proxies'", "PUPPETDB_USE_PROXIES", :false)

    purge_after_run =
        get_option_value(options[:puppetdb_purge_after_run],
          [:true, :false],
          "'purge packages and perform exhaustive cleanup after run'",
          "PUPPETDB_PURGE_AFTER_RUN", :false)

    package_build_host =
        get_option_value(options[:puppetdb_package_build_host],
          nil,
          "'hostname for package build output'",
          "PUPPETDB_PACKAGE_BUILD_HOST",
          "builds.puppetlabs.lan")

    package_repo_host =
        get_option_value(options[:puppetdb_package_repo_host],
          nil,
          "'hostname for yum/apt repos'",
          "PUPPETDB_PACKAGE_REPO_HOST",
          "neptune.puppetlabs.lan")

    package_repo_url =
        get_option_value(options[:puppetdb_package_repo_url],
          nil,
          "'base URL for yum/apt repos'",
          "PUPPETDB_PACKAGE_REPO_URL",
          "http://#{package_repo_host}/dev/puppetdb/master")

    puppetdb_repo_puppet = get_option_value(options[:puppetdb_repo_puppet],
      nil, "git repo for puppet source installs", "PUPPETDB_REPO_PUPPET", nil)

    puppetdb_repo_hiera = get_option_value(options[:puppetdb_repo_hiera],
      nil, "git repo for hiera source installs", "PUPPETDB_REPO_HIERA", nil)

    puppetdb_repo_facter = get_option_value(options[:puppetdb_repo_facter],
      nil, "git repo for facter source installs", "PUPPETDB_REPO_FACTER", nil)

    puppetdb_repo_puppetdb = get_option_value(options[:puppetdb_repo_puppetdb],
      nil, "git repo for puppetdb source installs", "PUPPETDB_REPO_PUPPETDB", nil)

    puppetdb_git_ref = get_option_value(options[:puppetdb_git_ref],
      nil, "git revision of puppetdb to test against", "REF", nil)

    @config = {
      :base_dir => base_dir,
      :acceptance_data_dir => File.join(base_dir, "acceptance", "data"),
      :os_families => os_families,
      :install_type => install_type,
      :install_mode => install_mode,
      :database => database,
      :validate_package_version => validate_package_version == :true,
      :expected_rpm_version => expected_rpm_version,
      :expected_deb_version => expected_deb_version,
      :use_proxies => use_proxies == :true,
      :purge_after_run => purge_after_run == :true,
      :package_build_host => package_build_host,
      :package_repo_host => package_repo_host,
      :package_repo_url => package_repo_url,
      :repo_puppet => puppetdb_repo_puppet,
      :repo_hiera => puppetdb_repo_hiera,
      :repo_facter => puppetdb_repo_facter,
      :repo_puppetdb => puppetdb_repo_puppetdb,
      :git_ref => puppetdb_git_ref,
    }

    pp_config = PP.pp(@config, "")

    Beaker::Log.notify "PuppetDB Acceptance Configuration:\n\n#{pp_config}\n\n"
  end

  class << self
    attr_reader :config
  end


  def self.get_option_value(value, legal_values, description,
    env_var_name = nil, default_value = nil)

    # we give precedence to any value explicitly specified in an options file,
    #  but we also allow environment variables to be used for
    #  puppetdb-specific settings
    value = (value || (env_var_name && ENV[env_var_name]) || default_value)
    if value
      value = value.to_sym
    end

    unless legal_values.nil? or legal_values.include?(value)
      raise ArgumentError, "Unsupported #{description} '#{value}'"
    end

    value
  end

  # Return the configuration hash initialized at the start with
  # initialize_test_config
  #
  # @return [Hash] configuration hash
  def test_config
    PuppetDBExtensions.config
  end

  # Return the fact set for a given hostname
  #
  # Relies on populate_facts to be ran first.
  #
  # @param host [String] hostname to retrieve facts for
  # @return [Hash] facts hash
  def facts(host)
    test_config[:facts][host]
  end

  # Populate the facts storage area of test_config
  #
  # @return [void]
  def populate_facts
    fact_data = hosts.inject({}) do |result, host|
      facts_raw = on host, "facter -y"
      facts = YAML.load(facts_raw.stdout)
      result[host.name] = facts
      result
    end

    test_config[:facts] = fact_data
    nil
  end

  def get_os_family(host)
    on(host, "which yum", :silent => true)
    if result.exit_code == 0
      on(host, "ls /etc/fedora-release", :silent => true)
      if result.exit_code == 2
        :redhat
      else
        :fedora
      end
    else
      :debian
    end
  end


  def puppetdb_confdir(host)
    if host.is_pe?
      "/etc/puppetlabs/puppetdb"
    else
      "/etc/puppetdb"
    end
  end

  def puppetdb_sharedir(host)
    if host.is_pe?
      "/opt/puppet/share/puppetdb"
    else
      "/usr/share/puppetdb"
    end
  end

  def puppetdb_sbin_dir(host)
    if host.is_pe?
      "/opt/puppet/sbin"
    else
      "/usr/sbin"
    end
  end

  def puppetdb_pids(host)
    java_bin = "java"
    jar_file = "puppetdb.jar"
    result = on host, %Q(ps -ef | grep "#{java_bin}" | grep "#{jar_file}" | grep " services -c " | awk '{print $2}')
    pids = result.stdout.chomp.split("\n")
    Beaker::Log.notify "PuppetDB PIDs appear to be: '#{pids}'"
    pids
  end

  def start_puppetdb(host)
    step "Starting PuppetDB" do
      if host.is_pe?
        on host, "service pe-puppetdb start"
      else
        on host, "service puppetdb start"
      end
      sleep_until_started(host)
    end
  end

  def sleep_until_started(host)
    # Hit an actual endpoint to ensure PuppetDB is up and not just the webserver.
    # Retry until an HTTP response code of 200 is received.
    curl_with_retries("start puppetdb", host,
                      "-s -w '%{http_code}' http://localhost:8080/v3/version -o /dev/null",
                      0, 120, 1, /200/)
    curl_with_retries("start puppetdb (ssl)", host,
                      "https://#{host.node_name}:8081/", [35, 60])
  end

  def get_package_version(host, version = nil)
    return version unless version.nil?

    ## These 'platform' values come from the acceptance config files, so
    ## we're relying entirely on naming conventions here.  Would be nicer
    ## to do this using lsb_release or something, but...
    if host['platform'].include?('el-5')
      "#{PuppetDBExtensions.config[:expected_rpm_version]}.el5"
    elsif host['platform'].include?('el-6')
      "#{PuppetDBExtensions.config[:expected_rpm_version]}.el6"
    elsif host['platform'].include?('el-7')
      "#{PuppetDBExtensions.config[:expected_rpm_version]}.el7"
    elsif host['platform'].include?('fedora')
      version_tag = host['platform'].match(/^fedora-(\d+)/)[1]
      "#{PuppetDBExtensions.config[:expected_rpm_version]}.fc#{version_tag}"
    elsif host['platform'].include?('ubuntu') or host['platform'].include?('debian')
      "#{PuppetDBExtensions.config[:expected_deb_version]}"
    else
      raise ArgumentError, "Unsupported platform: '#{host['platform']}'"
    end

  end

  def el5?(host)
    test_config[:os_families][host.name] == :redhat && fact_on(host, "lsbmajdistrelease") == '5'
  end

  def add_el5_postgres(host, manifest_string)
    if el5?(host)
      "class { 'postgresql::globals':
         client_package_name => 'postgresql84',
         server_package_name => 'postgresql84-server',
         devel_package_name  => 'postgresql84-devel',
         version => '8.4',
         bindir => '/usr/bin',
         service_name => 'postgresql',
         datadir => '/var/lib/pgsql/data',
         java_package_name => 'postgresql-jdbc',
         plperl_package_name => 'postgresql84-plperl',
         contrib_package_name => 'postgresql84-contrib'}
       #{manifest_string}"
    else
      manifest_string
    end
  end

  def install_puppetdb(host, db, version=nil)
    manifest = add_el5_postgres(host, "
    class { 'puppetdb':
      database             => '#{db}',
      manage_firewall      => false,
      puppetdb_version     => '#{get_package_version(host, version)}',
    }")

    apply_manifest_on(host, manifest)
    print_ini_files(host)
    sleep_until_started(host)
  end


  def validate_package_version(host)
    step "Verifying package version" do
      os = PuppetDBExtensions.config[:os_families][host.name]
      installed_version =
        case os
          when :debian
            result = on host, "dpkg-query --showformat \"\\${Version}\" --show puppetdb"
            result.stdout.strip
          when :redhat, :fedora
            result = on host, "rpm -q puppetdb --queryformat \"%{VERSION}-%{RELEASE}\""
            result.stdout.strip
          else
            raise ArgumentError, "Unsupported OS family: '#{os}'"
        end
      expected_version = get_package_version(host)

      Beaker::Log.notify "Expecting package version: '#{expected_version}', actual version: '#{installed_version}'"
      if installed_version != expected_version
        raise RuntimeError, "Installed version '#{installed_version}' did not match expected version '#{expected_version}'"
      end
    end
  end


  def install_puppetdb_termini(host, database, version=nil)
    # We pass 'restart_puppet' => false to prevent the module from trying to
    # manage the puppet master service, which isn't actually installed on the
    # acceptance nodes (they run puppet master from the CLI).
    manifest = <<-EOS
    class { 'puppetdb::master::config':
      puppetdb_server          => '#{database.node_name}',
      puppetdb_version         => '#{get_package_version(host, version)}',
      puppetdb_startup_timeout => 120,
      manage_report_processor  => true,
      enable_reports           => true,
      restart_puppet           => false,
    }
    EOS
    apply_manifest_on(host, manifest)
  end


  def print_ini_files(host)
    step "Print out jetty.ini for posterity" do
      on host, "cat /etc/puppetdb/conf.d/jetty.ini"
    end
    step "Print out database.ini for posterity" do
      on host, "cat /etc/puppetdb/conf.d/database.ini"
    end
  end

  def current_time_on(host)
    result = on host, %Q|date --rfc-2822|
    CGI.escape(Time.rfc2822(result.stdout).iso8601)
  end

  ############################################################################
  # NOTE: the following methods should only be called during run-from-source
  #  acceptance test runs.
  ############################################################################

  def install_postgres(host)
    Beaker::Log.notify "Installing postgres on #{host}"


    ############################################################################
    # NOTE: A lot of the differences between the PE and FOSS manifests here is
    #   only necessary because the puppetdb::database::postgresql module
    #   doesn't parameterize things like the service name. It would be nice
    #   to simplify this once we've added more paramters to the module.
    ############################################################################

    if host.is_pe?
      service_name = "pe-postgresql"
      db_name = "pe-puppetdb"
      db_user = "mYpdBu3r"
      db_pass = '~!@#$%^*-/ aZ'
      manifest = <<-EOS
      # get the pg server up and running
      $version = '9.2'
      class { 'postgresql':
        client_package_name => 'pe-postgresql',
        server_package_name => 'pe-postgresql-server',
        devel_package_name  => 'pe-postgresql-devel',
        java_package_name   => 'pe-postgresql-jdbc',
        datadir             => "/opt/puppet/var/lib/pgsql/${version}/data",
        confdir             => "/opt/puppet/var/lib/pgsql/${version}/data",
        bindir              => '/opt/puppet/bin',
        service_name        => 'pe-postgresql',
        user                => 'pe-postgres',
        group               => 'pe-postgres',
        locale              => 'en_US.UTF8',
        charset             => 'UTF8',
        run_initdb          => true,
        version             => $version
      } ->
      class { '::postgresql::server':
        service_name            => #{service_name},
        ip_mask_allow_all_users => '0.0.0.0/0',
      }
      # create the puppetdb database
      postgresql::server::db { '#{db_name}':
        user     => '#{db_user}',
        password => '#{db_pass}',
      }
      EOS
    else
      manifest = add_el5_postgres(host, "class { 'puppetdb::database::postgresql': }")
    end
    apply_manifest_on(host, manifest)
  end

  # Restart postgresql using Puppet, by notifying the postgresql::server::service
  # class, which should cause the service to restart.
  def restart_postgres(host)
    manifest = add_el5_postgres(host, "class { 'puppetdb::database::postgresql': }\n")
    manifest += <<-EOS
      notify { 'restarting postgresql': }~>
      Class['postgresql::server::service']
    EOS
    apply_manifest_on(host, manifest)
  end

  def install_puppetdb_via_rake(host)
    os = PuppetDBExtensions.config[:os_families][host.name]
    case os
      when :debian
        preinst = "debian/puppetdb.preinst install"
        postinst = "debian/puppetdb.postinst"
      when :redhat, :fedora
        preinst = "dev/redhat/redhat_dev_preinst install"
        postinst = "dev/redhat/redhat_dev_postinst install"
      else
        raise ArgumentError, "Unsupported OS family: '#{os}'"
    end

    on host, "rm -rf /etc/puppetdb/ssl"
    on host, "#{LeinCommandPrefix} rake package:bootstrap"
    on host, "#{LeinCommandPrefix} rake template"
    on host, "bash -x #{GitReposDir}/puppetdb/ext/files/#{preinst}"
    on host, "#{LeinCommandPrefix} rake install"
    on host, "bash -x #{GitReposDir}/puppetdb/ext/files/#{postinst}"

    step "Configure database.ini file" do
      manifest = add_el5_postgres(host, "
        $database = '#{PuppetDBExtensions.config[:database]}'
        class { 'puppetdb::server::database_ini':
          database => $database,
        }")

      apply_manifest_on(host, manifest)
    end

    print_ini_files(host)
  end

  def install_puppetdb_termini_via_rake(host, database)
    on host, "#{LeinCommandPrefix} rake sourceterminus"

    manifest = <<-EOS
      include puppetdb::master::storeconfigs
      class { 'puppetdb::master::puppetdb_conf':
        server => '#{database.node_name}',
      }
      include puppetdb::master::routes
      class { 'puppetdb::master::report_processor':
        enable => true,
      }
    EOS
    apply_manifest_on(host, manifest)
  end

  ###########################################################################


  def stop_puppetdb(host)
    pids = puppetdb_pids(host)

    if host.is_pe?
      on host, "service pe-puppetdb stop"
    else
      on host, "service puppetdb stop"
    end

    sleep_until_stopped(host, pids)
  end

  def sleep_until_stopped(host, pids)
    curl_with_retries("stop puppetdb", host, "http://localhost:8080", 7)
    stopped = false
    while (! stopped)
      Beaker::Log.notify("Waiting for pids #{pids} to exit")
      exit_codes = pids.map do |pid|
        result = on host, %Q(ps -p #{pid}), :acceptable_exit_codes => [0, 1]
        result.exit_code
      end
      stopped = exit_codes.all? { |x| x == 1}
    end
  end

  def restart_puppetdb(host)
    stop_puppetdb(host)
    start_puppetdb(host)
  end

  def clear_and_restart_puppetdb(host)
    stop_puppetdb(host)
    clear_database(host)
    start_puppetdb(host)
  end

  def sleep_until_queue_empty(host, timeout=60)
    metric = "org.apache.activemq:BrokerName=localhost,Type=Queue,Destination=com.puppetlabs.puppetdb.commands"
    queue_size = nil

    begin
      Timeout.timeout(timeout) do
        until queue_size == 0
          result = on host, %Q(curl http://localhost:8080/v3/metrics/mbean/#{CGI.escape(metric)} 2> /dev/null |awk -F"," '{for (i = 1; i <= NF; i++) { print $i } }' |grep QueueSize |awk -F ":" '{ print $2 }')
          queue_size = Integer(result.stdout.chomp)
        end
      end
    rescue Timeout::Error => e
      raise "Queue took longer than allowed #{timeout} seconds to empty"
    end
  end

  def apply_manifest_on(host, manifest_content)
    manifest_path = host.tmpfile("puppetdb_manifest.pp")
    create_remote_file(host, manifest_path, manifest_content)
    Beaker::Log.notify "Applying manifest on #{host}:\n\n#{manifest_content}"
    on host, puppet_apply("--detailed-exitcodes #{manifest_path}"), :acceptable_exit_codes => [0,2]
  end

  def modify_config_setting(host, config_file_name, section, setting, value)
    manifest_content = <<EOS
ini_setting {'puppetdb-config-#{setting}':
    path => '#{puppetdb_confdir(host)}/conf.d/#{config_file_name}',
    section => '#{section}',
    setting => '#{setting}',
    value => '#{value}'
}
EOS
    apply_manifest_on(host, manifest_content)
  end

  def curl_with_retries(desc, host, curl_args, desired_exit_codes, max_retries = 60, retry_interval = 1, expected_output = /.*/)
    desired_exit_codes = [desired_exit_codes].flatten
    result = on host, "curl --tlsv1 #{curl_args}", :acceptable_exit_codes => (0...127)
    num_retries = 0
    until desired_exit_codes.include?(exit_code) and (result.stdout =~ expected_output)
      sleep retry_interval
      result = on host, "curl --tlsv1 #{curl_args}", :acceptable_exit_codes => (0...127)
      num_retries += 1
      if (num_retries > max_retries)
        fail("Unable to #{desc}")
      end
    end
  end

  def clear_database(host)
    case PuppetDBExtensions.config[:database]
      when :postgres
        if host.is_pe?
          on host, 'su - pe-postgres -s "/bin/bash" -c "/opt/puppet/bin/dropdb pe-puppetdb"'
        else
          on host, 'su postgres -c "dropdb puppetdb"'
        end
        install_postgres(host)
      when :embedded
        on host, "rm -rf #{puppetdb_sharedir(host)}/db/*"
      else
        raise ArgumentError, "Unsupported database: '#{PuppetDBExtensions.config[:database]}'"
    end
  end

  #########################################################
  # PuppetDB export utility functions
  #########################################################
  # These are for comparing puppetdb export tarballs.
  # This seems like a pretty ridiculous place to define them,
  # but there are no other obvious choices that I see at the
  # moment.  Should consider moving them to a ruby utility
  # code folder in the main PuppetDB source tree if such a
  # thing ever materializes.

  # @param export_file1 [String] path to first export file
  # @param export_file2 [String] path to second export file
  # @param opts [Hash] comparison options
  # @option opts [Boolean] :catalogs compare catalog? defaults to true
  # @option opts [Boolean] :metadata compare metadata? defaults to true
  # @option opts [Boolean] :reports compare reports? defaults to true
  def compare_export_data(export_file1, export_file2, opts={})
    # Apply defaults
    opts = {
      :catalogs => true,
      :metadata => true,
      :reports => true,
      :facts => true
    }.merge(opts)

    # NOTE: I'm putting this tmpdir inside of cwd because I expect for that to
    #  be inside of the jenkins workspace, which I'm hoping means that it will
    #  be cleaned up regularly if we accidentally leave anything lying around
    tmpdir = "./puppetdb_export_test_tmp"
    FileUtils.rm_rf(tmpdir)
    export_dir1 = File.join(tmpdir, "export1", File.basename(export_file1, ".tar.gz"))
    export_dir2 = File.join(tmpdir, "export2", File.basename(export_file2, ".tar.gz"))
    FileUtils.mkdir_p(export_dir1)
    FileUtils.mkdir_p(export_dir2)

    `tar zxvf #{export_file1} -C #{export_dir1}`
    `tar zxvf #{export_file2} -C #{export_dir2}`

    export1_files = Set.new()
    Dir.glob("#{export_dir1}/**/*") do |f|
      relative_path = f.sub(/^#{export_dir1}\//, "")
      export1_files.add(relative_path)
      expected_path = File.join(export_dir2, relative_path)

      if(relative_path !~ /^puppetdb-bak\/facts.*/ || opts[:facts])
          assert(File.exists?(expected_path), "Export file '#{export_file2}' is missing entry '#{relative_path}'")
      end

      puts "Comparing file '#{relative_path}'"
      next if File.directory?(f)

      export_entry_type = get_export_entry_type(relative_path)
      case export_entry_type
        when :catalog
          compare_catalog(f, expected_path) if opts[:catalogs]
        when :metadata
          compare_metadata(f, expected_path) if opts[:metadata]
        when :report
          compare_report(f, expected_path) if opts[:reports]
        when :facts
          compare_facts(f, expected_path) if opts[:facts]
        when :unknown
          fail("Unrecognized file found in archive: '#{relative_path}'")
      end
    end

    export2_files = Set.new(
      Dir.glob("#{export_dir2}/**/*").map { |f| f.sub(/^#{Regexp.escape(export_dir2)}\//, "") })

    export1_files.delete_if{ |path| !opts[:facts] && /^puppetdb-bak\/facts.*/.match(path)}
    export2_files.delete_if{ |path| !opts[:facts] && /^puppetdb-bak\/facts.*/.match(path)}

    diff = export2_files - export1_files

    assert(diff.empty?, "Export file '#{export_file2}' contains extra file entries: '#{diff.to_a.join("', '")}'")

    FileUtils.rm_rf(tmpdir)
  end

  def get_export_entry_type(path)
    case path
      when "puppetdb-bak/export-metadata.json"
        :metadata
      when /^puppetdb-bak\/catalogs\/.*\.json$/
        :catalog
      when /^puppetdb-bak\/reports\/.*\.json$/
        :report
      when /^puppetdb-bak\/facts\/.*\.json$/
        :facts
      else
        :unknown
    end
  end

  def compare_facts(facts1_path, facts2_path)
    f1 = JSON.parse(File.read(facts1_path))
    f2 = JSON.parse(File.read(facts2_path))

    diff = hash_diff(f1, f2)

    if (diff)
      diff = JSON.pretty_generate(diff)
    end

    assert(diff == nil, "Catalogs '#{facts1_path}' and '#{facts2_path}' don't match!' Diff:\n#{diff}")
  end

  def compare_catalog(cat1_path, cat2_path)
    cat1 = munge_catalog_for_comparison(cat1_path)
    cat2 = munge_catalog_for_comparison(cat2_path)

    diff = hash_diff(cat1, cat2)
    if (diff)
      diff = JSON.pretty_generate(diff)
    end

    assert(diff == nil, "Catalogs '#{cat1_path}' and '#{cat2_path}' don't match!' Diff:\n#{diff}")
  end

  def compare_report(cat1_path, cat2_path)
    cat1 = munge_report_for_comparison(cat1_path)
    cat2 = munge_report_for_comparison(cat2_path)

    diff = hash_diff(cat1, cat2)
    if (diff)
      diff = JSON.pretty_generate(diff)
    end

    assert(diff == nil, "Reports '#{cat1_path}' and '#{cat2_path}' don't match!' Diff:\n#{diff}")
  end

  def compare_metadata(meta1_path, meta2_path)
    meta1 = munge_metadata_for_comparison(meta1_path)
    meta2 = munge_metadata_for_comparison(meta2_path)

    diff = hash_diff(meta1, meta2)

    assert(diff == nil, "Export metadata does not match!  Diff\n#{diff}")
  end

  def munge_metadata_for_comparison(meta_path)
    meta = JSON.parse(File.read(meta_path))
    meta.delete("timestamp")
    meta
  end

  def munge_resource_for_comparison(resource)
    resource['tags'] = Set.new(resource['tags'])
    resource
  end

  def munge_catalog_for_comparison(cat_path)
    meta = JSON.parse(File.read(cat_path))
    munged_resources = meta["resources"].map { |resource| munge_resource_for_comparison(resource) }
    meta["resources"] = Set.new(munged_resources)
    meta["edges"] = Set.new(meta["edges"])
    meta
  end

  def munge_report_for_comparison(cat_path)
    JSON.parse(File.read(cat_path))
  end

  def parse_json_with_error(input)
      begin
          facts = JSON.parse(input)
      rescue Exception => e
          facts = "#{e.message} on input '#{input}'"
      end
      return facts
  end

  ############################################################################
  # NOTE: This code was merged into beaker, however it does not work as desired.
  #   We need to get the version in beaker working as expected and then we can
  #   remove this version.
  #
  #   Temp copy of Justins new Puppet Master Methods
  ############################################################################

  # Restore puppet.conf from backup, if puppet.conf.bak exists.
  #
  # @api private
  def restore_puppet_conf host
    confdir = host.puppet['confdir']
    on host, "if [ -f #{confdir}/puppet.conf.bak ]; then " +
               "cat #{confdir}/puppet.conf.bak > " +
               "#{confdir}/puppet.conf; " +
               "rm -rf #{confdir}/puppet.conf.bak; " +
             "fi"
  end

  ##############################################################################
  # END_OF Temp Copy of Justins new Puppet Master Methods
  ##############################################################################

  ##############################################################################
  # Object diff functions
  ##############################################################################
  # This is horrible and really doesn't belong here, but I'm not sure where
  # else to put it.  I need a way to do a recursive diff of a hash (which may
  # contain nested objects whose type can be any of Hash, Array, Set, or a
  # scalar).  The hashes may be absolutely gigantic, so if they don't match,
  # I need a way to be able to show a small enough diff so that the user can
  # actually figure out what's going wrong (rather than dumping out the entire
  # gigantic string).  I searched for gems that could handle this and tried
  # 4 or 5 different things, and couldn't find anything that suited the task,
  # so I had to write my own.  This could use improvement, relocation, or
  # replacement with a gem if we ever find a suitable one.
  #
  # UPDATE: chatted with Justin about this and he suggests creating a special
  # puppetlabs-test-utils repo or similar and have that pulled down via
  # bundler, once the acceptance harness is accessible as a gem.  You know,
  # in "The Future".

  # JSON gem doesn't have native support for Set objects, so we have to
  # add this hack.
  class ::Set
    def to_json(arg)
      to_a.to_json(arg)
    end
  end


  def hash_diff(obj1, obj2)
    result =
      (obj1.keys | obj2.keys).inject({}) do |diff, k|
        if obj1[k] != obj2[k]
          objdiff = object_diff(obj1[k], obj2[k])
          if (objdiff)
            diff[k] = objdiff
          end
        end
        diff
      end
    (result == {}) ? nil : result
  end

  def array_diff(arr1, arr2)
    (0..([arr1.length, arr2.length].max)).inject([]) do |diff, i|
      objdiff = object_diff(arr1[i], arr2[i])
      if (objdiff)
        diff << objdiff
      end
      diff
    end
  end

  def set_diff(set1, set2)
    diff1 = set1 - set2
    diff2 = set2 - set1
    unless (diff1.empty? and diff2.empty?)
      [diff1, diff2]
    end
  end

  def object_diff(obj1, obj2)
    if (obj1.class != obj2.class)
      [obj1, obj2]
    else
      case obj1
        when Hash
          hash_diff(obj1, obj2)
        when Array
          array_diff(obj1, obj2)
        when Set
          set_diff(obj1, obj2)
        else
          (obj1 == obj2) ? nil : [obj1, obj2]
      end
    end
  end

  ##############################################################################
  # End Object diff functions
  ##############################################################################

  def install_puppet_from_package
    os_families = test_config[:os_families]
    hosts.each do |host|
      os = os_families[host.name]

      case os
      when :debian
        if options[:type] == 'aio' then
          on host, "apt-get install -y puppet-agent"
          on host, "apt-get install -y puppetserver"
        else
          on host, "apt-get install -y puppet puppetmaster-common"
        end
      # Puppet 3.7.4 is broken on fedora, pinning to 3.7.3 until it's fixed
      when :fedora
        on host, "yum install -y puppet-3.7.3"
      when :redhat
        if options[:type] == 'aio' then
          on host, "yum install -y java-1.7.0-openjdk"
          on host, "yum install -y puppet-agent"
          on host, "yum install -y puppetserver"
        else
          on host, "yum install -y puppet"
        end
      else
        raise ArgumentError, "Unsupported OS '#{os}'"
      end
    end
  end

  # This helper has been grabbed from beaker, and overriden with the opts
  # component so I can add a new 'refspec' functionality to allow a custom
  # refspec if required.
  #
  # Once this methodology is confirmed we should merge it back upstream.
  def install_from_git(host, path, repository, opts = {})
    name   = repository[:name]
    repo   = repository[:path]
    rev    = repository[:rev]

    target = "#{path}/#{name}"

    step "Clone #{repo} if needed" do
      on host, "test -d #{path} || mkdir -p #{path}"
      on host, "test -d #{target} || git clone #{repo} #{target}"
    end

    step "Update #{name} and check out revision #{rev}" do
      commands = ["cd #{target}",
                  "remote rm origin",
                  "remote add origin #{repo}",
                  "fetch origin #{opts[:refspec]}",
                  "clean -fdx",
                  "checkout -f #{rev}"]
      on host, commands.join(" && git ")
    end

    step "Install #{name} on the system" do
      # The solaris ruby IPS package has bindir set to /usr/ruby/1.8/bin.
      # However, this is not the path to which we want to deliver our
      # binaries. So if we are using solaris, we have to pass the bin and
      # sbin directories to the install.rb
      install_opts = ''
      install_opts = '--bindir=/usr/bin --sbindir=/usr/sbin' if
        host['platform'].include? 'solaris'

        on host,  "cd #{target} && " +
                  "if [ -f install.rb ]; then " +
                  "ruby ./install.rb #{install_opts}; " +
                  "else true; fi"
    end
  end

  def install_puppet_from_source
    os_families = test_config[:os_families]

    extend Beaker::DSL::InstallUtils

    source_path = Beaker::DSL::InstallUtils::SourcePath
    git_uri     = Beaker::DSL::InstallUtils::GitURI
    github_sig  = Beaker::DSL::InstallUtils::GitHubSig

    tmp_repositories = []

    repos = Hash[*test_config.select {|k, v| k =~ /^repo_/ and k != 'repo_puppetdb' }.flatten].values

    repos.each do |uri|
      raise(ArgumentError, "#{uri} is not recognized.") unless(uri =~ git_uri)
      tmp_repositories << extract_repo_info_from(uri)
    end

    repositories = order_packages(tmp_repositories)

    hosts.each_with_index do |host, index|
      os = os_families[host.name]

      case os
      when :redhat, :fedora
        on host, "yum install -y git-core ruby"
      when :debian
        on host, "apt-get install -y git ruby"
      else
        raise "OS #{os} not supported"
      end

      on host, "echo #{github_sig} >> $HOME/.ssh/known_hosts"

      repositories.each do |repository|
        step "Install #{repository[:name]}"
        install_from_git host, source_path, repository,
          :refspec => '+refs/pull/*:refs/remotes/origin/pr/*'
      end

      on host, "getent group puppet || groupadd puppet"
      on host, "getent passwd puppet || useradd puppet -g puppet -G puppet"
      on host, "mkdir -p /var/run/puppet"
      on host, "chown puppet:puppet /var/run/puppet"
    end
  end

  def install_puppet_conf
    hosts.each do |host|
      confdir = host.puppet['confdir']
      puppetconf = File.join(confdir, 'puppet.conf')

      on host, "mkdir -p #{confdir}"

      conf = IniFile.new
      conf['agent'] = {
        'server' => master,
      }
      if options[:is_puppetserver] then
        pidfile = '/var/run/puppetlabs/puppetserver/puppetserver.pid'
      else
        pidfile = master.puppet('master')['pidfile']
      end
      conf['master'] = {
        'pidfile' => pidfile,
      }
      if options[:type] == 'aio' then
        hostname = fact_on(host, "hostname")
        fqdn = fact_on(host, "fqdn")
        conf['master']['dns_alt_names']="puppet,#{hostname},#{fqdn},#{host.hostname}"
      end
      create_remote_file host, puppetconf, conf.to_s
    end
  end

  def install_puppet
    # If our :install_type is :pe then the harness has already installed puppet.
    case test_config[:install_type]
    when :package
      install_puppet_from_package
      install_puppet_conf
    when :git
      if test_config[:repo_puppet] then
        install_puppet_from_source
      else
        install_puppet_from_package
      end
      install_puppet_conf
    end
  end

  def create_remote_site_pp(host, manifest)
    testdir = host.tmpdir("remote-site-pp")
    manifest_file = "#{testdir}/environments/production/manifests/site.pp"
    apply_manifest_on(host, <<-PP)
    File {
      ensure => directory,
      mode => "0750",
      owner => #{master.puppet['user']},
      group => #{master.puppet['group']},
    }

    file {
      '#{testdir}':;
      '#{testdir}/environments':;
      '#{testdir}/environments/production':;
      '#{testdir}/environments/production/manifests':;
      '#{testdir}/environments/production/modules':;
    }
PP
    create_remote_file(host, manifest_file, manifest)
    remote_path = "#{testdir}/environments"
    on host, "chmod -R +rX #{testdir}"
    on host, "chown -R #{master.puppet['user']}:#{master.puppet['user']} #{testdir}"
    remote_path
  end

  def run_agents_with_new_site_pp(host, manifest, env_vars = {})

    manifest_path = create_remote_site_pp(host, manifest)
    with_puppet_running_on host, {
      'master' => {
        'storeconfigs' => 'true',
        'storeconfigs_backend' => 'puppetdb',
        'autosign' => 'true',
      },
      'main' => {
        'environmentpath' => manifest_path,
      }} do
      #only some of the opts work on puppet_agent, acceptable exit codes does not
      agents.each{ |agent| on agent, puppet_agent("--test --server #{host}", { 'ENV' => env_vars }), :acceptable_exit_codes => [0,2] }

    end
  end

  def puppetdb_vardir(host)
    if host.is_pe?
      "/var/lib/pe-puppetdb"
    else
      "/var/lib/puppetdb"
    end
  end

  def catalog_hash_debug_dir(host)
    puppetdb_vardir(host) + "/debug/catalog-hashes/"
  end

end

# oh dear.
Beaker::TestCase.send(:include, PuppetDBExtensions)
