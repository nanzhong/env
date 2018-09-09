function tmux-status-vpn
    set ip (ip addr show gpd0 ^ /dev/null | grep -Po 'inet \K[\d.]+')
    if test -z $ip
        set ip ...
    end
    echo "旅 $ip"
end
