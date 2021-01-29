function tmux-status-ip
    ip addr show eth0 | grep -Po 'inet \K[\d.]+'
end
