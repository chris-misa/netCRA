IF0=p4-veth0 
IF1=p4-veth1
ip link add name $IF0 type veth peer name $IF1
ip link set $IF0 up
ip link set $IF1 up
