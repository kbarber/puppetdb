test_name "load series of facts see how long it takes" do
  step "run benchmark" do
    on(database, "#{LeinCommandPrefix} lein run benchmark -F test-resources/puppetlabs/puppetdb/cli/benchmark/facts/ -n 5000 -c config.ini -N 100 --rand-perc 10")
  end

  step "wait max 6 minutes for completion" do
    sleep_until_queue_empty database, 360
  end

  step "check to ensure no commands were rejected or discarded" do
    assert_equal(0, command_processing_stats(database, "discarded")["Count"])
    assert_equal(0, command_processing_stats(database, "fatal")["Count"])
  end
end
