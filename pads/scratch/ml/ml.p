#if defined(FOR_CKIT)
typedef void * XDR;
void xdrmem_create(XDR *xin, void * buf, size_t s, int code);
int xdr_float(XDR *xin, Pfloat32* f);
int xdr_double(XDR *xin, Pfloat64* f);
enum{XDR_ENCODE, XDR_DECODE} xdr_codes;
#else
#include <rpc/rpc.h> 
#endif

// Assume argument pointers point to valid space
void Pstr2float(Pstring *src, Pbase_pd *src_pd, Pfloat32 *dest, Pbase_pd *dest_pd){
  XDR xin;
  *dest = 0;
  *dest_pd = *src_pd;  /* set destination parse descriptor from source */
  xdrmem_create(&xin, src->str, src->len, XDR_DECODE);
  if (!xdr_float(&xin, dest)) {
    // Error: XDR conversion failed
      dest_pd->nerr++;
      dest_pd->errCode = P_TRANSFORM_FAILED;
  };
};

void Pfloat2str(P_t *p, Pfloat32 *src, Pbase_pd *src_pd, Pstring *dest, Pbase_pd *dest_pd){
  XDR xout;
  char str[8];
  *dest_pd = *src_pd;
  xdrmem_create(&xout, str, 4, XDR_ENCODE);
  xdr_float(&xout, src);
  if (P_OK != Pstring_cstr_copy(p, dest, str, 4)) {
    // Error: copy into Pstring failed
      dest_pd->nerr++;
      dest_pd->errCode = P_TRANSFORM_FAILED;
  };
}


Ptrans XDR_float32{
 Pstr2float: Pstring_FW(:4:) <=> Pfloat32: Pfloat2str(:pads:);
};

// Assume argument pointers point to valid space
void Pstr2float64(Pstring *src, Pbase_pd *src_pd, Pfloat64 *dest, Pbase_pd *dest_pd){
  XDR xin;
  *dest = 0;
  *dest_pd = *src_pd;  /* set destination parse descriptor from source */
  xdrmem_create(&xin, src->str, src->len, XDR_DECODE);
  if (!xdr_double(&xin, dest)) {
    // Error: XDR conversion failed
      dest_pd->nerr++;
      dest_pd->errCode = P_TRANSFORM_FAILED;
  };
};

void Pfloat642str(P_t *p, Pfloat64 *src, Pbase_pd *src_pd, Pstring *dest, Pbase_pd *dest_pd){
  XDR xout;
  char str[8];
  *dest_pd = *src_pd;
  xdrmem_create(&xout, str, 8, XDR_ENCODE);
  xdr_double(&xout, src);
  if (P_OK != Pstring_cstr_copy(p, dest, str, 8)) {
    // Error: copy into Pstring failed
      dest_pd->nerr++;
      dest_pd->errCode = P_TRANSFORM_FAILED;
  };
}


Ptrans XDR_float64{
 Pstr2float64: Pstring_FW(:8:) <=> Pfloat64: Pfloat642str(:pads:);
};


Pstruct XDR_string{
  Psbh_uint32(:4:)  len;
  Pstring_FW(:len:) str;
};


// Assume argument pointers point to valid space
void Psbh_uint322ip(Puint32 *src, Pbase_pd *src_pd, Pip *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

void Pip2Psbh_uint322ip(Pip *src, Pbase_pd *src_pd, Puint32 *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};


Ptrans Psbh_ip{
  Psbh_uint322ip : Psbh_uint32(:4:) <=> Pip : Pip2Psbh_uint322ip;
};


// Assume argument pointers point to valid space
void uint322timestamp(Puint32 *src, Pbase_pd *src_pd, Ptimestamp *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

void timestamp2uint32(Ptimestamp *src, Pbase_pd *src_pd, Puint32 *dest, Pbase_pd *dest_pd){
  *dest_pd = *src_pd;
  *dest = *src;
};

Ptrans Psbh_timestamp{
  uint322timestamp : Psbh_uint32(:4:) <=> Ptimestamp(:' ':) : timestamp2uint32;
};

Ptrans Pb_timestamp{
  uint322timestamp : Pb_uint32 <=> Ptimestamp(:' ':) : timestamp2uint32;
};


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


