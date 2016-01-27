#!fish

# fbsd specific settings
set -x PACKAGESITE http://ftp-archive.freebsd.org/mirror/FreeBSD-Archive/old-releases/amd64/8.4-RELEASE/packages/Latest/

# path
set -U fish_user_paths {$HOME}/bin /usr/bin.real /usr/texbin

# env
set -x P4CONFIG .p4config
set -x P4DIFF colordiff
set -x P4MERGE emacs -merge
set -x BROWSER links
set -x NSADDR 10.217.212.6
set -x NSLOGIN nsroot@$NSADDR

# aliases
alias post-review "post-review --server=https://reviewboard.citrite.net --p4-port=sjcpperforce04.citrite.net:1666"
alias sshns "env TERM=xterm ssh $NSLOGIN"
alias less "less -r"
alias vi vim

# system settings
ulimit -c unlimited
