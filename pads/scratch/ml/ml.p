#include <aux.p>

typedef enum  {
   metric_user_defined = 0x00, /* gmetric message */
   metric_cpu_num,
   metric_cpu_speed,
   metric_mem_total,
   metric_swap_total,
   metric_boottime,
   metric_sys_clock,
   metric_machine_type,
   metric_os_name,
   metric_os_release,
   metric_cpu_user,
   metric_cpu_nice,
   metric_cpu_system,
   metric_cpu_idle,
   metric_cpu_aidle,
   metric_load_one,
   metric_load_five,
   metric_load_fifteen,
   metric_proc_run,
   metric_proc_total,
   metric_mem_free,
   metric_mem_shared,
   metric_mem_buffers,
   metric_mem_cached,
   metric_swap_free,
   metric_gexec,
   metric_heartbeat,
   metric_mtu,
   metric_location,
   metric_bytes_out,
   metric_bytes_in,
   metric_pkts_in,
   metric_pkts_out,
   metric_disk_total,
   metric_disk_free,
   metric_part_max_used,
   metric_cpu_wio,
   metric_bread_sec,
   metric_bwrite_sec,
   metric_lread_sec,
   metric_lwrite_sec,
   metric_rcache,
   metric_wcache,
   metric_phread_sec,
   metric_phwrite_sec,
   metric_cpu_intr,
   metric_cpu_sintr,
   metric_mem_arm,
   metric_mem_rm,
   metric_mem_avm,
   metric_mem_vm,
   GANGLIA_NUM_25_METRICS /* this should always directly follow the last 25 metric_* */
} Ganglia_message_formats;

Pstruct Ganglia_gmetric_message{
  XDR_string type;
  XDR_string name;
  XDR_string value;
  XDR_string units;
  Pb_uint32 slope;
  Pb_uint32 tmax;
  Pb_uint32 dmax;
};

Punion Ganglia_message (:Ganglia_message_formats id:) {
  Pswitch (id) {
   Pcase metric_user_defined:
    Ganglia_gmetric_message gmetric;

  Pcase metric_cpu_num:      Psbh_uint16(:2:) cpu;
 
  Pcase metric_cpu_speed:    Psbh_uint32(:4:) cpu_speed;
  Pcase metric_mem_total:    Psbh_uint32(:4:) mem_total;   
  Pcase metric_swap_total:   Psbh_uint32(:4:) swap_total;   
  Pcase metric_boottime:     Psbh_uint32(:4:) boottime;
  Pcase metric_sys_clock:    Psbh_uint32(:4:) sys_clock;
  Pcase metric_proc_run:     Psbh_uint32(:4:) proc_run;
  Pcase metric_proc_total:   Psbh_uint32(:4:) proc_total;
  Pcase metric_mem_free:     Psbh_uint32(:4:) mem_free;
  Pcase metric_mem_shared:   Psbh_uint32(:4:) mem_shared;
  Pcase metric_mem_buffers:  Psbh_uint32(:4:) mem_buffers;
  Pcase metric_mem_cached:   Psbh_uint32(:4:) mem_cached;
  Pcase metric_swap_free:    Psbh_uint32(:4:) swap_free;
  Pcase metric_heartbeat:    Psbh_timestamp heartbeat;
  Pcase metric_mtu:          Psbh_uint32(:4:) mtu;
  Pcase metric_mem_arm:      Psbh_uint32(:4:) mem_arm;
  Pcase metric_mem_rm:       Psbh_uint32(:4:) mem_rm;
  Pcase metric_mem_avm:      Psbh_uint32(:4:) mem_avm;
  Pcase metric_mem_vm:       Psbh_uint32(:4:) mem_vm;

  Pcase metric_machine_type: XDR_string machine_type;
  Pcase metric_os_name:      XDR_string os_name;
  Pcase metric_os_release:   XDR_string os_release;
  Pcase metric_gexec:        XDR_string gexec;
  Pcase metric_location:     XDR_string location;

  Pcase metric_cpu_user:     XDR_float32 cpu_user;
  Pcase metric_cpu_nice:     XDR_float32 cpu_nice;
  Pcase metric_cpu_system:   XDR_float32 cpu_system;
  Pcase metric_cpu_idle:     XDR_float32 cpu_idle;
  Pcase metric_cpu_aidle:    XDR_float32 cpu_aidle;
  Pcase metric_load_one:     XDR_float32 load_one;
  Pcase metric_load_five:    XDR_float32 load_five;
  Pcase metric_load_fifteen: XDR_float32 load_fifteen;
  Pcase metric_bytes_in:     XDR_float32 bytes_in;
  Pcase metric_bytes_out:    XDR_float32 bytes_out;
  Pcase metric_pkts_in:      XDR_float32 pkts_in;
  Pcase metric_pkts_out:     XDR_float32 pkts_out;
  Pcase metric_part_max_used:XDR_float32 part_max_used;
  Pcase metric_cpu_wio:      XDR_float32 cpu_wio;
  Pcase metric_bread_sec:    XDR_float32 bread_sec;
  Pcase metric_bwrite_sec:   XDR_float32 bwrite_sec;
  Pcase metric_lread_sec:    XDR_float32 lread_sec;
  Pcase metric_lwrite_sec:   XDR_float32 lwrite_sec;
  Pcase metric_rcache:       XDR_float32 rcache;
  Pcase metric_wcache:       XDR_float32 wcache;
  Pcase metric_phread_sec:   XDR_float32 phread_sec;
  Pcase metric_phwrite_sec:  XDR_float32 phwrite_sec;
  Pcase metric_cpu_intr:     XDR_float32 cpu_intr;
  Pcase metric_cpu_sintr:    XDR_float32 cpu_sintr;

  Pcase metric_disk_total:   XDR_float64 disk_total;
  Pcase metric_disk_free:    XDR_float64 disk_free;

  Pdefault:                  Pcompute Puint8 other = 0; // define Pvoid
  }
};

Pstruct payload_t{
  Psbh_uint32(:4:) which;
  Ganglia_message(:which:) payload;
};

Pstruct libpcapFrame_t{
  Pb_timestamp packetCaptureTime;
  Pb_uint32 microSecsSinceCapture;
  Pb_uint32 wireSize;
  Pb_uint32 fileSize; /-- wire and file sizes are often equal
};

// We should add this type as a base type with accum, etc support
Parray mac_t {
  Psbh_uint8(:1:)[6];
};

Pstruct ethernet_t{
  mac_t src;
  mac_t dst;
  Psbh_uint16(:2:) protocol : protocol == 0x0800; /-- 0800 means IP
};

Pstruct ip_t{
  Psbh_uint8(:1:)  versionAndLength;
  Pcompute  char version = versionAndLength /16 : version == 4;
  Pcompute  char headerLength = versionAndLength % 16;
  Psbh_uint8(:1:)  TOS;
  Psbh_uint16(:2:) totalLength;
  Psbh_uint16(:2:) id;
  Psbh_uint16(:2:) flagsAndOffset;
  Psbh_uint8(:1:)  TTL;
  Psbh_uint8(:1:)  protocol: protocol == 0x11; /-- UDP
  Psbh_uint16(:2:) headerChecksum; // add code to verify
  Psbh_ip          src; 
  Psbh_ip          dst;
  // if necessary, add options here, determined by size of header minus 20 (fixed size)
};

Pstruct udp_t{
  Psbh_uint16(:2:) srcPort;
  Psbh_uint16(:2:) dstPort;
  Psbh_uint16(:2:) length;  /-- length of udp header and body in bytes
  Psbh_uint16(:2:) checkSum;
};

// should support physical sizeof construct
Pstruct packet_t {
  libpcapFrame_t               libpcap;
  Pcompute size_t start = position.offset;
  ethernet_t                   ethernet;
  ip_t                         IP;
  udp_t                        udp;
  payload_t                    payload;
  Pcompute size_t middle = position.offset;
  Pb_uint8[libpcap.wireSize - (middle-start)]       padding;
};

Pstruct libpcapHeader_t{
  Pendian Pb_uint32 magicNumber : magicNumber == 0xa1b2c3d4;
  Pb_uint16 majorVersion; /-- should be 2
  Pb_uint16 minorVersion; /-- should be 4
  Pb_uint32 timeZoneOffset      : timeZoneOffset == 0;
  Pb_uint32 timeStampAccuracy   : timeStampAccuracy == 0;
  Pb_uint32 snapShotLength;     /-- 65535 indicates no limit
  Pb_uint32 linkLayerType;
};

Pstruct lipcap_t{
  libpcapHeader_t h;
  packet_t[] packets;
}


