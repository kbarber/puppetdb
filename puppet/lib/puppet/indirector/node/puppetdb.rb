require 'puppet/node'
require 'puppet/indirector/puppetdb_cqrs'
require 'puppet/util/puppetdb'

class Puppet::Node::Puppetdb < Puppet::Indirector::PuppetdbCqrs
  include Puppet::Util::Puppetdb

  def find(request)
  end

  def save(request)
  end

  def destroy(request)
    submit_command(request.key, request.key.to_pson, CommandDeactivateNode, 1)
  end
end
