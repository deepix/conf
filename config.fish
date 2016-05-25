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
set -x P4USER deepaknag

# env: go
set -x GOROOT ~/go
set -x CC gcc
set -x GOPATH ~/p4_fbsd84_2/main/rs_111_20_3_cpx_ulfd/usr.src/netscaler/nsulfd/go

# path
set -gx PATH $GOROOT/bin /root/bin /sbin /bin /usr/sbin /usr/bin /usr/local/sbin /usr/local/bin

# aliases
alias post-review "post-review --server=https://reviewboard.citrite.net --p4-port=$P4PORT"
alias sshns "env TERM=xterm ssh $NSLOGIN"
alias less "less -r"
alias vi vim

# system settings
ulimit -c unlimited
