function tmux-status-vpn
    set addr (ip addr show gpd0 2> /dev/null)
    if echo $addr | grep -q "state DOWN"
        echo "旅 ..."
        return 1
    end
    set ip (echo $addr | grep -Po 'inet \K[\d.]+')
    if test -z $ip
        set ip ...
    end
    echo "旅 $ip"
end
