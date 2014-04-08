%global realname puppetdb
%global realversion <%= Pkg::Config.version %>
%global rpmversion <%= Pkg::Config.rpmversion %>

%define __jar_repack 0

# On PE, we use sitelibdir
%global puppet_libdir     %(PATH=/opt/puppet/bin:$PATH ruby -rrbconfig -e "puts RbConfig::CONFIG['sitelibdir']")

%global _with_systemd  0
%global _with_sysvinit 0

# EL > 7 and SLES >= 12 use systemd
%if 0%{?rhel} >= 7 || 0%{?sles_version} >= 12
%global _with_systemd 1
%endif

%if 0%{?rhel} && 0%{?rhel} < 7
%global _with_sysvinit 1
%global _old_el        1
%endif

%if 0%{?sles_version} && 0%{?sles_version} < 12
%global _with_sysvinit 1
%global _old_sles      1
%endif

# These macros are not always defined on much older rpm-based systems
%global  _sharedstatedir /var/lib
%global  _realsysconfdir /etc
%if 0%{?suse_version}
%global  _initddir       %{_realsysconfdir}/init.d
%else
%global  _initddir       %{_realsysconfdir}/rc.d/init.d
%endif
%global  _rundir         /var/run

%global p_vendor pe
%global _orig_prefix %{_prefix}
%global name_prefix  %{p_vendor}-
%global _sysconfdir /etc/puppetlabs
%define _lib lib
%define _prefix /opt/puppet
%define _libexecdir %{_prefix}/libexec

Name:          pe-puppetdb
Version:       <%= Pkg::Config.rpmversion %>
Release:       <%= Pkg::Config.rpmrelease %>%{?dist}
BuildRoot:     %{_tmppath}/%{realname}-%{version}-%{release}-root-%(%{__id_u} -n)

Summary:       Puppet Centralized Storage Daemon
License:       PL Commercial
URL:           http://puppetlabs.com/puppet/puppet-enterprise
Source0:       %{name}-%{realversion}.tar.gz


%if 0%{?suse_version}
Group:         System/Daemons
%else
Group:         System Environment/Daemons
%endif

BuildRequires: pe-facter >= 1.7.0, pe-puppet
BuildRequires: pe-rubygem-rake
BuildRequires: pe-ruby

Requires:      pe-puppet >= 2.7.12

BuildArch:     noarch
%if 0%{?suse_version}
BuildRequires: aaa_base
BuildRequires: unzip
BuildRequires: sles-release
Requires:      aaa_base
Requires:      pwdutils
Requires:      procps
%else
BuildRequires: /usr/sbin/useradd
%endif
%if 0%{?_with_systemd}
# Required for %%post, %%preun, %%postun
Requires:       systemd
BuildRequires:  systemd
%endif
%if  0%{?_old_el}
# Required for %%post and %%preun
Requires:       chkconfig
Requires:       initscripts
%endif
BuildRequires: pe-java
Requires:      pe-java
Requires:      logrotate

%description
Puppet Centralized Storage.

%package terminus
Summary: Puppet terminus files to connect to PuppetDB
%if 0%{?suse_version}
Group: System/Libraries
%else
Group: Development/Libraries
%endif
Requires: pe-puppet >= 2.7.12


%description terminus
Connect Puppet to PuppetDB by setting up a terminus for PuppetDB.

%prep
%setup -q -n %{name}-%{realversion}


%build

%install
%if 0%{?suse_version}
export NO_BRP_CHECK_BYTECODE_VERSION=true
%endif

rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/%{_initddir}

# Explicitly set environment variable LANG,
# because otherwise it gets set to 'C' and that
# is wrong, wrong, wrong. There may be a better
# way to handle this, so this may change.
export LANG=en_US.UTF-8

PATH=/opt/puppet/bin:$PATH rake install PARAMS_FILE= DESTDIR=$RPM_BUILD_ROOT PE_BUILD=true
PATH=/opt/puppet/bin:$PATH rake terminus PARAMS_FILE= DESTDIR=$RPM_BUILD_ROOT PE_BUILD=true

mkdir -p $RPM_BUILD_ROOT/%{_localstatedir}/log/%{name}
mkdir -p $RPM_BUILD_ROOT/%{_rundir}/%{name}
mkdir -p $RPM_BUILD_ROOT/%{_libexecdir}/%{name}
touch  $RPM_BUILD_ROOT/%{_localstatedir}/log/%{name}/%{name}.log

%if 0%{?fedora} >= 16 || 0%{?rhel} >= 7 || 0%{?sles_version} >= 12
sed -i '/notifempty/a\    su %{name} %{name}' $RPM_BUILD_ROOT/%{_realsysconfdir}/logrotate.d/%{name}
%endif

%clean
rm -rf $RPM_BUILD_ROOT

%pre

# Add PuppetDB user
getent group %{name} > /dev/null || groupadd -r %{name}
getent passwd %{name} > /dev/null || \
useradd -r -g %{name} -d <%= @install_dir %> -s /sbin/nologin \
     -c "PuppetDB daemon"  %{name}

%post
export PATH=/opt/puppet/bin:$PATH
%if 0%{?_with_systemd}
# Always reload the .service files if using systemd, in case they have changed.
/usr/bin/systemctl daemon-reload
%endif
%if 0%{?_with_sysvinit}
# If this is an install (as opposed to an upgrade)...
if [ "$1" = "1" ]; then
  # Register the puppetDB service
  /sbin/chkconfig --add %{name}
fi
%endif

<%= @sbin_dir -%>/puppetdb ssl-setup

<%= ERB.new(File.read("ext/templates/directory_perms.erb")).result %>


%preun
# If this is an uninstall (as opposed to an upgrade) then
#  we want to shut down and disable the service.
if [ "$1" = "0" ] ; then
%if 0%{?_with_systemd}
    /usr/bin/systemctl stop %{name}.service
    /usr/bin/systemctl disable %{name}.service
%endif
%if 0%{?_with_sysvinit}
    /sbin/service %{name} stop >/dev/null 2>&1
    /sbin/chkconfig --del %{name}
%endif
fi

%postun
# Remove the rundir if this is an uninstall (as opposed to an upgrade)...
if [ "$1" = "0" ]; then
    rm -rf %{_rundir}/%{name} || :
%if 0%{?_with_systemd}
    # Restart systemd after the service file is removed
    /usr/bin/systemctl daemon-reload || :
%endif
fi

# If this is an upgrade (as opposed to an install) then we need to check
# and restart the service if it is running.
if [ "$1" = "1" ] ; then
%if 0%{?_with_systemd}
        /usr/bin/systemctl condrestart %{name}.service >/dev/null 2>&1 || :
%endif
%if 0%{?_with_sysvinit}
        /sbin/service %{name} status >/dev/null 2>&1 && /sbin/service %{name} restart >/dev/null 2>&1 || :
%endif
fi


%files
%defattr(-, root, root)
%doc *.md
%doc documentation
%if 0%{?suse_version}
%dir %{_sysconfdir}/%{realname}
%dir %{_sysconfdir}/%{realname}/conf.d
%endif
%config(noreplace)%{_sysconfdir}/%{realname}/conf.d/config.ini
%config(noreplace)%{_sysconfdir}/%{realname}/log4j.properties
%config(noreplace)%{_sysconfdir}/%{realname}/conf.d/database.ini
%config(noreplace)%{_sysconfdir}/%{realname}/conf.d/jetty.ini
%config(noreplace)%{_sysconfdir}/%{realname}/conf.d/repl.ini
%config(noreplace)%{_realsysconfdir}/logrotate.d/%{name}
%config(noreplace)%{_realsysconfdir}/sysconfig/%{name}
%if 0%{?_with_systemd}
%{_unitdir}/%{name}.service
%else
%{_initddir}/%{name}
%endif
%{_sbindir}/puppetdb-ssl-setup
%{_sbindir}/puppetdb-foreground
%{_sbindir}/puppetdb-import
%{_sbindir}/puppetdb-export
%{_sbindir}/puppetdb-anonymize
%{_sbindir}/puppetdb
%dir %{_libexecdir}/%{name}
%{_libexecdir}/%{realname}/puppetdb-ssl-setup
%{_libexecdir}/%{realname}/puppetdb-foreground
%{_libexecdir}/%{realname}/puppetdb-import
%{_libexecdir}/%{realname}/puppetdb-export
%{_libexecdir}/%{realname}/puppetdb-anonymize
%{_libexecdir}/%{realname}/%{name}.env
%{_datadir}/%{realname}
%{_datadir}/%{realname}/state
%dir %{_localstatedir}/log/%{name}
%ghost %{_localstatedir}/log/%{name}/%{name}.log
%ghost %{_rundir}/%{name}


%files terminus
%defattr(-, root, root)
%{puppet_libdir}/puppet/application/storeconfigs.rb
%{puppet_libdir}/puppet/face/node/deactivate.rb
%{puppet_libdir}/puppet/face/node/status.rb
%{puppet_libdir}/puppet/face/storeconfigs.rb
%{puppet_libdir}/puppet/indirector/catalog/puppetdb.rb
%{puppet_libdir}/puppet/indirector/facts/puppetdb.rb
%{puppet_libdir}/puppet/indirector/facts/puppetdb_apply.rb
%{puppet_libdir}/puppet/indirector/node/puppetdb.rb
%{puppet_libdir}/puppet/indirector/resource/puppetdb.rb
%{puppet_libdir}/puppet/reports/puppetdb.rb
%{puppet_libdir}/puppet/util/puppetdb.rb
%{puppet_libdir}/puppet/util/puppetdb/char_encoding.rb
%{puppet_libdir}/puppet/util/puppetdb/command.rb
%{puppet_libdir}/puppet/util/puppetdb/command_names.rb
%{puppet_libdir}/puppet/util/puppetdb/config.rb
%{puppet_libdir}/puppet/util/puppetdb/blacklist.rb

%changelog
<%
require 'socket'
hostname = Socket.gethostname
-%>
* <%= Time.now.strftime("%a %b %d %Y") %> <%= ENV['USER'] -%> <<%= ENV['USER'].strip -%>@<%= hostname -%>> - <%= Pkg::Config.rpmversion %>-<%= Pkg::Config.rpmrelease %>
- Autobuild from Rake task

* Mon Apr 02 2012 Michael Stahnke <stahnma@puppetlabs.com> - 0.1.0-1
- Initial Packaging
