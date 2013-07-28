require 'net/http'
require 'uri'

require 'puppet/network/http'
require 'puppet/network/http_pool'
require 'puppet/network/http/api/v1'
require 'puppet/network/http/compression'

# Access objects via REST, with custom handling for PuppetDB command and query
# seperation.
class Puppet::Indirector::PuppetdbCqrs < Puppet::Indirector::Terminus
  include Puppet::Network::HTTP::API::V1
  include Puppet::Network::HTTP::Compression.module

  # Provide appropriate headers.
  def headers
    add_accept_encoding({"Accept" => model.supported_formats.join(", ")})
  end

  def network(request, type = :command)
    server = request.server
    port = request.port

    case type
    when :command
      server ||= self.class.command_server
      port ||= self.class.command_port
    when :query
      server ||=  self.class.query_server
      port ||= self.class.query_port
    end
    Puppet::Network::HTTP::Connection.new(server, port)
  end

  def http_get(request, path, headers = nil, *args)
    http_request(:get, request, path, headers, *args)
  end

  def http_post(request, path, data, headers = nil, *args)
    http_request(:post, request, path, data, headers, *args)
  end

  def http_head(request, path, headers = nil, *args)
    http_request(:head, request, path, headers, *args)
  end

  def http_delete(request, path, headers = nil, *args)
    http_request(:delete, request, path, headers, *args)
  end

  def http_put(request, path, data, headers = nil, *args)
    http_request(:put, request, path, data, headers, *args)
  end

  def http_request(method, request, *args)
    type = case method
    when :get, :head
      :query
    when :post, :delete, :put
      :command
    end
    conn = network(request, type)
    conn.send(method, *args)
  end

  def find(request)
    raise NotImplementedError
  end

  def head(request)
    raise NotImplementedError
  end

  def search(request)
    raise NotImplementedError
  end

  def destroy(request)
    raise NotImplementedError
  end

  def save(request)
    raise NotImplementedError
  end
end
