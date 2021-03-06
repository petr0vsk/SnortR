#!/bin/sh
# конвертирование трафика из pcap файла в ascii с помощью утилиты tshark
# при запуске скрипта указывается два аргумента имя исходного pcap-файла и имя целевого файла
if [ $# -ne 2 ]
then
	echo "Let's try: pcap_to_txt.sh from_file.pcap to_file.txt "
else
tshark -r $1 -Tfields -E separator=, -E header=y -E quote=d -e frame.number -e frame.time -e frame.time_delta -e frame.len -e frame.protocols -e eth.dst -e eth.src -e eth.dst_resolved -e eth.src_resolved -e eth.type -e ip.version -e ip.len -e ip.id -e ip.flags -e ip.frag_offset -e ip.ttl -e ip.proto -e ip.src -e ip.dst -e tcp.srcport -e tcp.dstport -e tcp.stream -e tcp.len -e tcp.seq -e tcp.hdr_len -e tcp.flags.res -e tcp.flags.ns -e tcp.flags.cwr -e tcp.flags.ecn -e tcp.flags.urg -e tcp.flags.ack -e tcp.flags.push -e tcp.flags.reset -e tcp.flags.syn  -e _ws.expert.message -e _ws.expert.severity -e _ws.expert.group -e tcp.flags.fin > $2
fi
