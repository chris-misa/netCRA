SWITCH_EXE=simple_switch
IF=p4-veth0
$SWITCH_EXE --log-console -i 0@$IF $1
