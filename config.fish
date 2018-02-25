# Path to Oh My Fish install.
set -q XDG_DATA_HOME
  and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
  or set -gx OMF_PATH "$HOME/.local/share/omf"

# Load Oh My Fish configuration.
source $OMF_PATH/init.fish
#!fish

# fbsd specific settings
set -x PACKAGESITE http://ftp-archive.freebsd.org/mirror/FreeBSD-Archive/old-releases/amd64/8.4-RELEASE/packages/Latest/

# env
set -x P4CONFIG .p4config
set -x P4DIFF colordiff
set -x P4MERGE "emacs -merge"
set -x BROWSER links
set -x NSADDR 10.217.216.208
set -x NSLOGIN nsroot@$NSADDR
set -x P4PORT SJCPperforce04.citrite.net:1666
set -gx P4USER deepakna
set -gx PYTHONPATH $PYTHONPATH /usr/local/Cellar/apache-spark/2.1.1/libexec/python
set -gx SPARK_HOME /usr/local/Cellar/apache-spark/2.1.1/libexec
set -gx HADOOP_PREFIX /usr/local/Cellar/hadoop/2.7.2/libexec
set -gx HADOOP_CONF_DIR /usr/local/Cellar/hadoop/2.7.2/libexec/etc/hadoop
set -gx HOMEBREW_NO_AUTO_UPDATE 1

set -x CC gcc

# path
set -U fish_user_paths ~/bin
set -gx PATH /root/bin /sbin /bin /usr/sbin /usr/bin /usr/local/sbin /usr/local/bin

# aliases
alias sshns "env TERM=xterm ssh $NSLOGIN"
alias less "less -r"
alias vi vim

# system settings
ulimit -c unlimited

set -gx theme_color_scheme solarized-light

alias proxy-on "ssh -fN rdp"
alias proxy-check 'ssh -O check rdp'
alias proxy-off 'ssh -O exit rdp'

