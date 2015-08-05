require 'puppet/util/puppetdb/http'

Puppet::Parser::Functions::newfunction(:pdb_query, :type => :rvalue, :doc =>
  "foo
  bar") do |args|
  query = CGI.escape(args[0])

  begin
    response = Puppet::Util::Puppetdb::Http.action("/pdb/query/v4/?query=#{query}") do |http_instance, path |
      http_instance.get(path, {'Accept' => 'application/json'})
    end

    return JSON.load(response.body)
  rescue => e
    raise Puppet::Error, "Query failed: #{e}"
  end
end
