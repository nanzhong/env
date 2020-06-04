function tmux-status-ip
    set ip (ip addr show eth0 | grep -Po 'inet \K[\d.]+')
    echo " $ip"
end
