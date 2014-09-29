test_name "load series of facts see how long it takes" do
  step "run benchmark" do
    on(master, "#{LeinCommandPrefix} lein run benchmark -h")
  end
end
