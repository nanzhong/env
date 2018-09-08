function tmux-status-system
    set cpu (top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print 100 - $1}')
    set mem (free -m | awk '/Mem:/ { printf("%3.1f%%", $3/$2*100) }' | sed "s/%//")
    set disk (df -h / | awk '/\// {print $(NF-1)}' | sed "s/%/.0/")

    set net_usage (string split "\n" (ifdata -bips -bops eth0))
    if test $net_usage[1] -ge 1024
        set net_in (echo $net_usage[1] | numfmt --to=iec)
    else
        set net_in "$net_usage[1]B"
    end
    set net_out (echo $net_usage[2] | numfmt --to=iec)
    if test $net_usage[2] -ge 1024
        set net_out (echo $net_usage[2] | numfmt --to=iec)
    else
        set net_out "$net_usage[2]B"
    end

    echo "⌈$cpu%⌋ ⌈$mem%⌋ ⌈$disk%⌋ ⌈$net_in⌋ ⌈$net_out⌋"
end

function tmux-status-ip
    set ip (ip addr show eth0 | grep -Po 'inet \K[\d.]+')
    echo "酪 $ip"
end

function tmux-status-vpn
    set ip (ip addr show gpd0 ^ /dev/null | grep -Po 'inet \K[\d.]+')
    if test -z $ip
        set ip ...
    end
    echo "旅 $ip"
end

function tmux-status-minikube
    set minikube_ip (minikube ip ^ /dev/null)
    if test -z $minikube_ip
        set minikube_ip ...
    end
    echo "歷 $minikube_ip"
end
