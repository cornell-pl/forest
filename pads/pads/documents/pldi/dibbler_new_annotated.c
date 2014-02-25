#include "pads-internal.h"
#include "dibbler_new.h"
Perror_t zip_t_init (P_t *pads,zip_t *rep)
{
  return P_OK;
}
Perror_t zip_t_pd_init (P_t *pads,zip_t_pd *pd)
{
  return P_OK;
}
Perror_t zip_t_cleanup (P_t *pads,zip_t *rep)
{
  return P_OK;
}
Perror_t zip_t_pd_cleanup (P_t *pads,zip_t_pd *pd)
{
  return P_OK;
}
Perror_t zip_t_copy (P_t *pads,zip_t *rep_dst,zip_t *rep_src)
{
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(zip_t));
  return P_OK;
}
Perror_t zip_t_pd_copy (P_t *pads,zip_t_pd *pd_dst,zip_t_pd *pd_src)
{
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(zip_t_pd));
  return P_OK;
}
void zip_t_m_init (P_t *pads,zip_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(zip_t_m));
}
Perror_t zip_t_read (P_t *pads,zip_t_m *m,zip_t_pd *pd,zip_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("zip_t_read",m,pd,rep);
  PCGEN_TYPEDEF_READ ("zip_t_read",Puint32_read (pads,&(m->base),pd,rep));
  return ((pd->nerr)==0) ? P_OK : P_ERR;
}
int is_zip_t (zip_t *rep)
{
  return 1;
}
Perror_t zip_t_acc_init (P_t *pads,zip_t_acc *acc)
{
  return Puint32_acc_init (pads,acc);
}
Perror_t zip_t_acc_reset (P_t *pads,zip_t_acc *acc)
{
  return Puint32_acc_reset (pads,acc);
}
Perror_t zip_t_acc_cleanup (P_t *pads,zip_t_acc *acc)
{
  return Puint32_acc_cleanup (pads,acc);
}
Perror_t zip_t_acc_add (P_t *pads,zip_t_acc *acc,zip_t_pd *pd,zip_t *rep)
{
  return Puint32_acc_add (pads,acc,pd,rep);
}
Perror_t zip_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,zip_t_acc *acc)
{
  PCGEN_TYPEDEF_ACC_REP2IO ("typedef zip_t","Puint32",Puint32_acc_report2io (pads,outstr,prefix,what,nst,acc));
}
Perror_t zip_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,zip_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = zip_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t zip_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zip_t_pd *pd,zip_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("zip_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  tlen_PCGEN_ = Puint32_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,pd,rep);
  PCGEN_FINAL_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t zip_t_write2io (P_t *pads,Sfio_t *io,zip_t_pd *pd,zip_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("zip_t_write2io",zip_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t zip_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zip_t_pd *pd,zip_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("zip_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  if (!tag) 
    tag = "zip_t";
  tlen_PCGEN_ = Puint32_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,pd,rep,tag,indent);
  PCGEN_FINAL_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t zip_t_write_xml_2io (P_t *pads,Sfio_t *io,zip_t_pd *pd,zip_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("zip_t_write_xml_2io",zip_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t zip_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zip_t_m *m,zip_t_pd *pd,zip_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_TYPEDEF_FMT2BUF_FINAL_INIT ("zip_t_fmt2buf_final");
  PCGEN_FMT2BUF_TYPEDEF (Puint32_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->base),pd,rep));
  return length_PCGEN_;
}
ssize_t zip_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zip_t_m *m,zip_t_pd *pd,zip_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("zip_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"zip_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return zip_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t zip_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,zip_t_m *m,zip_t_pd *pd,zip_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("zip_t_fmt2io",zip_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
Perror_t pn_t_init (P_t *pads,pn_t *rep)
{
  return P_OK;
}
Perror_t pn_t_pd_init (P_t *pads,pn_t_pd *pd)
{
  return P_OK;
}
Perror_t pn_t_cleanup (P_t *pads,pn_t *rep)
{
  return P_OK;
}
Perror_t pn_t_pd_cleanup (P_t *pads,pn_t_pd *pd)
{
  return P_OK;
}
Perror_t pn_t_copy (P_t *pads,pn_t *rep_dst,pn_t *rep_src)
{
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(pn_t));
  return P_OK;
}
Perror_t pn_t_pd_copy (P_t *pads,pn_t_pd *pd_dst,pn_t_pd *pd_src)
{
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(pn_t_pd));
  return P_OK;
}
void pn_t_m_init (P_t *pads,pn_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(pn_t_m));
}
Perror_t pn_t_read (P_t *pads,pn_t_m *m,pn_t_pd *pd,pn_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("pn_t_read",m,pd,rep);
  PCGEN_TYPEDEF_READ ("pn_t_read",Puint64_read (pads,&(m->base),pd,rep));
  return ((pd->nerr)==0) ? P_OK : P_ERR;
}
int is_pn_t (pn_t *rep)
{
  return 1;
}
Perror_t pn_t_acc_init (P_t *pads,pn_t_acc *acc)
{
  return Puint64_acc_init (pads,acc);
}
Perror_t pn_t_acc_reset (P_t *pads,pn_t_acc *acc)
{
  return Puint64_acc_reset (pads,acc);
}
Perror_t pn_t_acc_cleanup (P_t *pads,pn_t_acc *acc)
{
  return Puint64_acc_cleanup (pads,acc);
}
Perror_t pn_t_acc_add (P_t *pads,pn_t_acc *acc,pn_t_pd *pd,pn_t *rep)
{
  return Puint64_acc_add (pads,acc,pd,rep);
}
Perror_t pn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,pn_t_acc *acc)
{
  PCGEN_TYPEDEF_ACC_REP2IO ("typedef pn_t","Puint64",Puint64_acc_report2io (pads,outstr,prefix,what,nst,acc));
}
Perror_t pn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,pn_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = pn_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t pn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,pn_t_pd *pd,pn_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("pn_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  tlen_PCGEN_ = Puint64_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,pd,rep);
  PCGEN_FINAL_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t pn_t_write2io (P_t *pads,Sfio_t *io,pn_t_pd *pd,pn_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("pn_t_write2io",pn_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t pn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,pn_t_pd *pd,pn_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("pn_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  if (!tag) 
    tag = "pn_t";
  tlen_PCGEN_ = Puint64_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,pd,rep,tag,indent);
  PCGEN_FINAL_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t pn_t_write_xml_2io (P_t *pads,Sfio_t *io,pn_t_pd *pd,pn_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("pn_t_write_xml_2io",pn_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t pn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,pn_t_m *m,pn_t_pd *pd,pn_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_TYPEDEF_FMT2BUF_FINAL_INIT ("pn_t_fmt2buf_final");
  PCGEN_FMT2BUF_TYPEDEF (Puint64_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->base),pd,rep));
  return length_PCGEN_;
}
ssize_t pn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,pn_t_m *m,pn_t_pd *pd,pn_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("pn_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"pn_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return pn_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t pn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,pn_t_m *m,pn_t_pd *pd,pn_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("pn_t_fmt2io",pn_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
Perror_t summary_header_t_init (P_t *pads,summary_header_t *rep)
{
  return P_OK;
}
Perror_t summary_header_t_pd_init (P_t *pads,summary_header_t_pd *pd)
{
  return P_OK;
}
Perror_t summary_header_t_cleanup (P_t *pads,summary_header_t *rep)
{
  return P_OK;
}
Perror_t summary_header_t_pd_cleanup (P_t *pads,summary_header_t_pd *pd)
{
  return P_OK;
}
Perror_t summary_header_t_copy (P_t *pads,summary_header_t *rep_dst,summary_header_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("summary_header_t_copy",rep_src,rep_dst);
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(summary_header_t));
  return P_OK;
}
Perror_t summary_header_t_pd_copy (P_t *pads,summary_header_t_pd *pd_dst,summary_header_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("summary_header_t_pd_copy",pd_src,pd_dst);
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(summary_header_t_pd));
  return P_OK;
}
void summary_header_t_m_init (P_t *pads,summary_header_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(summary_header_t_m));
}
Perror_t summary_header_t_read (P_t *pads,summary_header_t_m *m,summary_header_t_pd *pd,summary_header_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("summary_header_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  // Read delimeter field: "0|"
  PCGEN_STRUCT_READ_FIRST_STR_LIT ("summary_header_t_read","0|",2);
  // Read field 'tstamp'
  PCGEN_STRUCT_READ_NEXT ("summary_header_t_read",tstamp,Puint32_read (pads,&(m->tstamp),&(pd->tstamp),&(rep->tstamp)),_NOOP);
  PCGEN_FIND_EOR ("summary_header_t_read");
  return ((pd->nerr)==0) ? P_OK : P_ERR;
}
int is_summary_header_t (summary_header_t *rep)
{
  return 1;
}
Perror_t summary_header_t_acc_init (P_t *pads,summary_header_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_init (pads,&(acc->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t summary_header_t_acc_reset (P_t *pads,summary_header_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_reset (pads,&(acc->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t summary_header_t_acc_cleanup (P_t *pads,summary_header_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t summary_header_t_acc_add (P_t *pads,summary_header_t_acc *acc,summary_header_t_pd *pd,summary_header_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = P_NO_ERR;
  if (P_ERR==Puint32_acc_add (pads,&(acc->nerr),&tpd,&(pd->nerr))) 
    {
      nerr++;
    }
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Puint32_acc_add (pads,&(acc->tstamp),&(pd->tstamp),&(rep->tstamp))) 
        {
          nerr++;
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t summary_header_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,summary_header_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "struct summary_header_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_STRUCT_ACC_REP_NOVALS ();
  if (P_ERR==P_nerr_acc_report2io (pads,outstr,"Errors","errors",-1,&(acc->nerr))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.tstamp",prefix);
  if (P_ERR==Puint32_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->tstamp))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t summary_header_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,summary_header_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = summary_header_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t summary_header_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,summary_header_t_pd *pd,summary_header_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("summary_header_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  tlen_PCGEN_ = PDCI_io_rec_open_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,"summary_header_t_write2buf");
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pcstr_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,"0|");
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint32_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->tstamp),&(rep->tstamp));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = PDCI_io_rec_close_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,buf,length_PCGEN_,"summary_header_t_write2buf");
  PCGEN_FINAL_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t summary_header_t_write2io (P_t *pads,Sfio_t *io,summary_header_t_pd *pd,summary_header_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("summary_header_t_write2io",summary_header_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t summary_header_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,summary_header_t_pd *pd,summary_header_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("summary_header_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("summary_header_t");
  PCGEN_STRUCT_PD_XML_OUT ();
  tlen_PCGEN_ = Puint32_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->tstamp),&(rep->tstamp),"tstamp",indent+2);
  PCGEN_TLEN_UPDATES ();
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t summary_header_t_write_xml_2io (P_t *pads,Sfio_t *io,summary_header_t_pd *pd,summary_header_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("summary_header_t_write_xml_2io",summary_header_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t summary_header_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,summary_header_t_m *m,summary_header_t_pd *pd,summary_header_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("summary_header_t_fmt2buf_final");
  PCGEN_FMT2BUF_STRUCT_FIELD ("Puint32_fmt2buf",Puint32_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->tstamp),&(pd->tstamp),&(rep->tstamp)));
  PCGEN_FMT2BUF_FIX_LAST ();
  PCGEN_FMT2BUF_RECORD ("summary_header_t_fmt2buf_final");
  return length_PCGEN_;
}
ssize_t summary_header_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,summary_header_t_m *m,summary_header_t_pd *pd,summary_header_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("summary_header_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"summary_header_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return summary_header_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t summary_header_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,summary_header_t_m *m,summary_header_t_pd *pd,summary_header_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("summary_header_t_fmt2io",summary_header_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
Perror_t no_ramp_t_init (P_t *pads,no_ramp_t *rep)
{
  return P_OK;
}
Perror_t no_ramp_t_pd_init (P_t *pads,no_ramp_t_pd *pd)
{
  return P_OK;
}
Perror_t no_ramp_t_cleanup (P_t *pads,no_ramp_t *rep)
{
  return P_OK;
}
Perror_t no_ramp_t_pd_cleanup (P_t *pads,no_ramp_t_pd *pd)
{
  return P_OK;
}
Perror_t no_ramp_t_copy (P_t *pads,no_ramp_t *rep_dst,no_ramp_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("no_ramp_t_copy",rep_src,rep_dst);
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(no_ramp_t));
  return P_OK;
}
Perror_t no_ramp_t_pd_copy (P_t *pads,no_ramp_t_pd *pd_dst,no_ramp_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("no_ramp_t_pd_copy",pd_src,pd_dst);
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(no_ramp_t_pd));
  return P_OK;
}
void no_ramp_t_m_init (P_t *pads,no_ramp_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(no_ramp_t_m));
}
Perror_t no_ramp_t_read (P_t *pads,no_ramp_t_m *m,no_ramp_t_pd *pd,no_ramp_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("no_ramp_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  // Read delimeter field: "no_ii"
  PCGEN_STRUCT_READ_FIRST_STR_LIT ("no_ramp_t_read","no_ii",5);
  // Read field 'id'
  PCGEN_STRUCT_READ_NEXT ("no_ramp_t_read",id,Puint64_read (pads,&(m->id),&(pd->id),&(rep->id)),_NOOP);
  return ((pd->nerr)==0) ? P_OK : P_ERR;
}
int is_no_ramp_t (no_ramp_t *rep)
{
  return 1;
}
Perror_t no_ramp_t_acc_init (P_t *pads,no_ramp_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Puint64_acc_init (pads,&(acc->id))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t no_ramp_t_acc_reset (P_t *pads,no_ramp_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Puint64_acc_reset (pads,&(acc->id))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t no_ramp_t_acc_cleanup (P_t *pads,no_ramp_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Puint64_acc_cleanup (pads,&(acc->id))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t no_ramp_t_acc_add (P_t *pads,no_ramp_t_acc *acc,no_ramp_t_pd *pd,no_ramp_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = P_NO_ERR;
  if (P_ERR==Puint32_acc_add (pads,&(acc->nerr),&tpd,&(pd->nerr))) 
    {
      nerr++;
    }
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Puint64_acc_add (pads,&(acc->id),&(pd->id),&(rep->id))) 
        {
          nerr++;
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t no_ramp_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,no_ramp_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "struct no_ramp_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_STRUCT_ACC_REP_NOVALS ();
  if (P_ERR==P_nerr_acc_report2io (pads,outstr,"Errors","errors",-1,&(acc->nerr))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.id",prefix);
  if (P_ERR==Puint64_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->id))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t no_ramp_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,no_ramp_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = no_ramp_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t no_ramp_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,no_ramp_t_pd *pd,no_ramp_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("no_ramp_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  tlen_PCGEN_ = Pcstr_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,"no_ii");
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint64_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->id),&(rep->id));
  PCGEN_FINAL_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t no_ramp_t_write2io (P_t *pads,Sfio_t *io,no_ramp_t_pd *pd,no_ramp_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("no_ramp_t_write2io",no_ramp_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t no_ramp_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,no_ramp_t_pd *pd,no_ramp_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("no_ramp_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("no_ramp_t");
  PCGEN_STRUCT_PD_XML_OUT ();
  tlen_PCGEN_ = Puint64_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->id),&(rep->id),"id",indent+2);
  PCGEN_TLEN_UPDATES ();
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t no_ramp_t_write_xml_2io (P_t *pads,Sfio_t *io,no_ramp_t_pd *pd,no_ramp_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("no_ramp_t_write_xml_2io",no_ramp_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t no_ramp_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,no_ramp_t_m *m,no_ramp_t_pd *pd,no_ramp_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("no_ramp_t_fmt2buf_final");
  PCGEN_FMT2BUF_STRUCT_FIELD ("Puint64_fmt2buf",Puint64_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->id),&(pd->id),&(rep->id)));
  PCGEN_FMT2BUF_FIX_LAST ();
  return length_PCGEN_;
}
ssize_t no_ramp_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,no_ramp_t_m *m,no_ramp_t_pd *pd,no_ramp_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("no_ramp_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"no_ramp_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return no_ramp_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t no_ramp_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,no_ramp_t_m *m,no_ramp_t_pd *pd,no_ramp_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("no_ramp_t_fmt2io",no_ramp_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
ssize_t dib_ramp_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,dib_ramp_t_pd *pd,dib_ramp_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("dib_ramp_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  switch (rep->tag)
    {
      
    case ramp: 
      {
        tlen_PCGEN_ = Puint64_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).ramp),&((rep->val).ramp));
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case genRamp: 
      {
        tlen_PCGEN_ = no_ramp_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).genRamp),&((rep->val).genRamp));
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case dib_ramp_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t dib_ramp_t_write2io (P_t *pads,Sfio_t *io,dib_ramp_t_pd *pd,dib_ramp_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("dib_ramp_t_write2io",dib_ramp_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t dib_ramp_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,dib_ramp_t_pd *pd,dib_ramp_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("dib_ramp_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("dib_ramp_t");
  PCGEN_UNION_PD_XML_OUT ();
  switch (rep->tag)
    {
      
    case ramp: 
      {
        tlen_PCGEN_ = Puint64_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).ramp),&((rep->val).ramp),"ramp",indent+2);
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case genRamp: 
      {
        tlen_PCGEN_ = no_ramp_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).genRamp),&((rep->val).genRamp),"genRamp",indent+2);
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case dib_ramp_t_err: 
      {
        // error case
        break;
      }
    }
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t dib_ramp_t_write_xml_2io (P_t *pads,Sfio_t *io,dib_ramp_t_pd *pd,dib_ramp_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("dib_ramp_t_write_xml_2io",dib_ramp_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
char const *dib_ramp_t_tag2str (dib_ramp_t_tag which)
{
  switch (which)
    {
      
    case ramp: 
      {
        return "ramp";
      }
      
    case genRamp: 
      {
        return "genRamp";
      }
      
    default: 
      {
        return "*unknown_tag*";
      }
    }
}
Perror_t dib_ramp_t_init (P_t *pads,dib_ramp_t *rep)
{
  return P_OK;
}
Perror_t dib_ramp_t_pd_init (P_t *pads,dib_ramp_t_pd *pd)
{
  return P_OK;
}
Perror_t dib_ramp_t_cleanup (P_t *pads,dib_ramp_t *rep)
{
  return P_OK;
}
Perror_t dib_ramp_t_pd_cleanup (P_t *pads,dib_ramp_t_pd *pd)
{
  return P_OK;
}
Perror_t dib_ramp_t_copy (P_t *pads,dib_ramp_t *rep_dst,dib_ramp_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("dib_ramp_t_copy",rep_src,rep_dst);
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(dib_ramp_t));
  return P_OK;
}
Perror_t dib_ramp_t_pd_copy (P_t *pads,dib_ramp_t_pd *pd_dst,dib_ramp_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("dib_ramp_t_pd_copy",pd_src,pd_dst);
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(dib_ramp_t_pd));
  return P_OK;
}
void dib_ramp_t_m_init (P_t *pads,dib_ramp_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(dib_ramp_t_m));
}
Perror_t dib_ramp_t_read (P_t *pads,dib_ramp_t_m *m,dib_ramp_t_pd *pd,dib_ramp_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("dib_ramp_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    {
      PCGEN_UNION_READ_SETUP_STAT ("dib_ramp_t_read",ramp,dib_ramp_t_cleanup,dib_ramp_t_init,dib_ramp_t_copy,dib_ramp_t_pd_cleanup,dib_ramp_t_pd_init,dib_ramp_t_pd_copy);
      // Read branch 'ramp'
      PCGEN_UNION_READ_STAT ("dib_ramp_t_read","ramp",ramp,dib_ramp_t_cleanup,dib_ramp_t_init,dib_ramp_t_copy,dib_ramp_t_pd_cleanup,dib_ramp_t_pd_init,dib_ramp_t_pd_copy,Puint64_read (pads,&(m->ramp),&((pd->val).ramp),&((rep->val).ramp)),dib_ramp_t_write_xml_2io (pads,sfstderr,pd,rep,"dib_ramp_t",4),_NOOP);
      // Read branch 'genRamp'
      PCGEN_UNION_READ_STAT ("dib_ramp_t_read","genRamp",genRamp,dib_ramp_t_cleanup,dib_ramp_t_init,dib_ramp_t_copy,dib_ramp_t_pd_cleanup,dib_ramp_t_pd_init,dib_ramp_t_pd_copy,no_ramp_t_read (pads,&(m->genRamp),&((pd->val).genRamp),&((rep->val).genRamp)),dib_ramp_t_write_xml_2io (pads,sfstderr,pd,rep,"dib_ramp_t",4),_NOOP);
      // Failed to match any branch of union dib_ramp_t
      PCGEN_UNION_READ_CHECK_FAILED ("dib_ramp_t_read","dib_ramp_t",dib_ramp_t_err);
      
    branches_done: 
      {
      }
      
    final_check: 
      {
      }
    }
    return ((pd->nerr)==0) ? P_OK : P_ERR;
  }
}
int is_dib_ramp_t (dib_ramp_t *rep)
{
  int isValid=1;
  switch (rep->tag)
    {
      
    case ramp: 
      {
        // PADS type has no is_ function and there is no user constraint
        break;
      }
      
    case genRamp: 
      {
        isValid = is_no_ramp_t (&((rep->val).genRamp));
        break;
      }
      
    case dib_ramp_t_err: 
      {
        // error case
        {
          isValid = 0;
          break;
        }
      }
    }
  return isValid;
}
Perror_t dib_ramp_t_acc_init (P_t *pads,dib_ramp_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==Puint64_acc_init (pads,&(acc->ramp))) 
    {
      nerr++;
    }
  if (P_ERR==no_ramp_t_acc_init (pads,&(acc->genRamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t dib_ramp_t_acc_reset (P_t *pads,dib_ramp_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==Puint64_acc_reset (pads,&(acc->ramp))) 
    {
      nerr++;
    }
  if (P_ERR==no_ramp_t_acc_reset (pads,&(acc->genRamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t dib_ramp_t_acc_cleanup (P_t *pads,dib_ramp_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==Puint64_acc_cleanup (pads,&(acc->ramp))) 
    {
      nerr++;
    }
  if (P_ERR==no_ramp_t_acc_cleanup (pads,&(acc->genRamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t dib_ramp_t_acc_add (P_t *pads,dib_ramp_t_acc *acc,dib_ramp_t_pd *pd,dib_ramp_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = (((pd->errCode)==P_UNION_MATCH_ERR) ? P_UNION_MATCH_ERR : P_NO_ERR);
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Pint32_acc_add (pads,&(acc->tag),&tpd,(Pint32 *) (&(rep->tag)))) 
        {
          nerr++;
        }
      switch (rep->tag)
        {
          
        case ramp: 
          {
            if (P_ERR==Puint64_acc_add (pads,&(acc->ramp),&((pd->val).ramp),&((rep->val).ramp))) 
              {
                nerr++;
              }
            break;
          }
          
        case genRamp: 
          {
            if (P_ERR==no_ramp_t_acc_add (pads,&(acc->genRamp),&((pd->val).genRamp),&((rep->val).genRamp))) 
              {
                nerr++;
              }
            break;
          }
          
        case dib_ramp_t_err: 
          {
            // error case
            break;
          }
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t dib_ramp_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,dib_ramp_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "union dib_ramp_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_UNION_ACC_REP_NOVALS ();
  if (P_ERR==Pint32_acc_map_report2io (pads,outstr,"Union tag","tag",-1,(Pint32_map_fn) dib_ramp_t_tag2str,&(acc->tag))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each tag arm of %s]\n",prefix);
  sfprintf (tmpstr,"%s.ramp",prefix);
  if (P_ERR==Puint64_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->ramp))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.genRamp",prefix);
  if (P_ERR==no_ramp_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->genRamp))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t dib_ramp_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,dib_ramp_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = dib_ramp_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t dib_ramp_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,dib_ramp_t_m *m,dib_ramp_t_pd *pd,dib_ramp_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("dib_ramp_t_fmt2buf_final");
  switch (rep->tag)
    {
      
    case ramp: 
      {
        PCGEN_FMT2BUF_UNION ("Puint64_fmt2buf",Puint64_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->ramp),&((pd->val).ramp),&((rep->val).ramp)),"ramp");
        break;
      }
      
    case genRamp: 
      {
        PCGEN_FMT2BUF_UNION ("no_ramp_t_fmt2buf",no_ramp_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->genRamp),&((pd->val).genRamp),&((rep->val).genRamp)),"genRamp");
        break;
      }
      
    case dib_ramp_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t dib_ramp_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,dib_ramp_t_m *m,dib_ramp_t_pd *pd,dib_ramp_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("dib_ramp_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"dib_ramp_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return dib_ramp_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t dib_ramp_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,dib_ramp_t_m *m,dib_ramp_t_pd *pd,dib_ramp_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("dib_ramp_t_fmt2io",dib_ramp_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
ssize_t service_tn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("service_tn_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  switch (rep->tag)
    {
      
    case some_service_tn_t: 
      {
        tlen_PCGEN_ = pn_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_service_tn_t),&((rep->val).some_service_tn_t));
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_service_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case service_tn_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t service_tn_t_write2io (P_t *pads,Sfio_t *io,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("service_tn_t_write2io",service_tn_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep,order_num,att_order_num,ord_version));
  return -1;
}
ssize_t service_tn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,service_tn_t_pd *pd,service_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("service_tn_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("service_tn_t");
  PCGEN_UNION_PD_XML_OUT ();
  switch (rep->tag)
    {
      
    case some_service_tn_t: 
      {
        tlen_PCGEN_ = pn_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_service_tn_t),&((rep->val).some_service_tn_t),"some_service_tn_t",indent+2);
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_service_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case service_tn_t_err: 
      {
        // error case
        break;
      }
    }
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t service_tn_t_write_xml_2io (P_t *pads,Sfio_t *io,service_tn_t_pd *pd,service_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("service_tn_t_write_xml_2io",service_tn_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent,order_num,att_order_num,ord_version));
  return -1;
}
char const *service_tn_t_tag2str (service_tn_t_tag which)
{
  switch (which)
    {
      
    case some_service_tn_t: 
      {
        return "some_service_tn_t";
      }
      
    case none_service_tn_t: 
      {
        return "none_service_tn_t";
      }
      
    default: 
      {
        return "*unknown_tag*";
      }
    }
}
Perror_t service_tn_t_init (P_t *pads,service_tn_t *rep)
{
  return P_OK;
}
Perror_t service_tn_t_pd_init (P_t *pads,service_tn_t_pd *pd)
{
  return P_OK;
}
Perror_t service_tn_t_cleanup (P_t *pads,service_tn_t *rep)
{
  return P_OK;
}
Perror_t service_tn_t_pd_cleanup (P_t *pads,service_tn_t_pd *pd)
{
  return P_OK;
}
Perror_t service_tn_t_copy (P_t *pads,service_tn_t *rep_dst,service_tn_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("service_tn_t_copy",rep_src,rep_dst);
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(service_tn_t));
  return P_OK;
}
Perror_t service_tn_t_pd_copy (P_t *pads,service_tn_t_pd *pd_dst,service_tn_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("service_tn_t_pd_copy",pd_src,pd_dst);
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(service_tn_t_pd));
  return P_OK;
}
void service_tn_t_m_init (P_t *pads,service_tn_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(service_tn_t_m));
}
Perror_t service_tn_t_read (P_t *pads,service_tn_t_m *m,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version)
{
  PDCI_IODISC_3P_CHECKS ("service_tn_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    Puint32 none_service_tn_t_Ptmp_;
    {
      PCGEN_UNION_READ_SETUP_STAT ("service_tn_t_read",some_service_tn_t,service_tn_t_cleanup,service_tn_t_init,service_tn_t_copy,service_tn_t_pd_cleanup,service_tn_t_pd_init,service_tn_t_pd_copy);
      // Read branch 'some_service_tn_t'
      PCGEN_UNION_READ_STAT ("service_tn_t_read","some_service_tn_t",some_service_tn_t,service_tn_t_cleanup,service_tn_t_init,service_tn_t_copy,service_tn_t_pd_cleanup,service_tn_t_pd_init,service_tn_t_pd_copy,pn_t_read (pads,&(m->some_service_tn_t),&((pd->val).some_service_tn_t),&((rep->val).some_service_tn_t)),service_tn_t_write_xml_2io (pads,sfstderr,pd,rep,"service_tn_t",4,order_num,att_order_num,ord_version),_NOOP);
      // Pcompute branch 'none_service_tn_t'
      PCGEN_UNION_READ_MAN_STAT_VIRT_PRE ("service_tn_t_read",none_service_tn_t,service_tn_t_init,service_tn_t_pd_init);
      none_service_tn_t_Ptmp_ = 0;
      PCGEN_UNION_READ_MAN_STAT_POST ("service_tn_t_read",service_tn_t_copy,service_tn_t_cleanup,service_tn_t_pd_copy,service_tn_t_pd_cleanup);
      // Failed to match any branch of union service_tn_t
      PCGEN_UNION_READ_CHECK_FAILED ("service_tn_t_read","service_tn_t",service_tn_t_err);
      
    branches_done: 
      {
      }
      
    final_check: 
      {
      }
    }
    return ((pd->nerr)==0) ? P_OK : P_ERR;
  }
}
int is_service_tn_t (service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version)
{
  int isValid=1;
  switch (rep->tag)
    {
      
    case some_service_tn_t: 
      {
        isValid = is_pn_t (&((rep->val).some_service_tn_t));
        break;
      }
      
    case none_service_tn_t: 
      {
        // Pomit branch: no user constraint
        break;
      }
      
    case service_tn_t_err: 
      {
        // error case
        {
          isValid = 0;
          break;
        }
      }
    }
  return isValid;
}
Perror_t service_tn_t_acc_init (P_t *pads,service_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_init (pads,&(acc->some_service_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t service_tn_t_acc_reset (P_t *pads,service_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_reset (pads,&(acc->some_service_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t service_tn_t_acc_cleanup (P_t *pads,service_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_cleanup (pads,&(acc->some_service_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t service_tn_t_acc_add (P_t *pads,service_tn_t_acc *acc,service_tn_t_pd *pd,service_tn_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = (((pd->errCode)==P_UNION_MATCH_ERR) ? P_UNION_MATCH_ERR : P_NO_ERR);
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Pint32_acc_add (pads,&(acc->tag),&tpd,(Pint32 *) (&(rep->tag)))) 
        {
          nerr++;
        }
      switch (rep->tag)
        {
          
        case some_service_tn_t: 
          {
            if (P_ERR==pn_t_acc_add (pads,&(acc->some_service_tn_t),&((pd->val).some_service_tn_t),&((rep->val).some_service_tn_t))) 
              {
                nerr++;
              }
            break;
          }
          
        case none_service_tn_t: 
          {
            // Pomit branch: cannot accumulate
            break;
          }
          
        case service_tn_t_err: 
          {
            // error case
            break;
          }
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t service_tn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,service_tn_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "opt service_tn_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_UNION_ACC_REP_NOVALS ();
  if (P_ERR==Pint32_acc_map_report2io (pads,outstr,"Opt tag","tag",-1,(Pint32_map_fn) service_tn_t_tag2str,&(acc->tag))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each tag arm of %s]\n",prefix);
  sfprintf (tmpstr,"%s.some_service_tn_t",prefix);
  if (P_ERR==pn_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->some_service_tn_t))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  // Pomit branch: cannot accumulate
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t service_tn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,service_tn_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = service_tn_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t service_tn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,service_tn_t_m *m,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("service_tn_t_fmt2buf_final");
  switch (rep->tag)
    {
      
    case some_service_tn_t: 
      {
        PCGEN_FMT2BUF_UNION ("pn_t_fmt2buf",pn_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->some_service_tn_t),&((pd->val).some_service_tn_t),&((rep->val).some_service_tn_t)),"some_service_tn_t");
        break;
      }
      
    case none_service_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case service_tn_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t service_tn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,service_tn_t_m *m,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("service_tn_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"service_tn_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version));
  return service_tn_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version);
}
ssize_t service_tn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,service_tn_t_m *m,service_tn_t_pd *pd,service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("service_tn_t_fmt2io",service_tn_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version));
  return -1;
}
ssize_t billing_tn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("billing_tn_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  switch (rep->tag)
    {
      
    case some_billing_tn_t: 
      {
        tlen_PCGEN_ = pn_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_billing_tn_t),&((rep->val).some_billing_tn_t));
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_billing_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case billing_tn_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t billing_tn_t_write2io (P_t *pads,Sfio_t *io,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("billing_tn_t_write2io",billing_tn_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep,order_num,att_order_num,ord_version,service_tn));
  return -1;
}
ssize_t billing_tn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,billing_tn_t_pd *pd,billing_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("billing_tn_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("billing_tn_t");
  PCGEN_UNION_PD_XML_OUT ();
  switch (rep->tag)
    {
      
    case some_billing_tn_t: 
      {
        tlen_PCGEN_ = pn_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_billing_tn_t),&((rep->val).some_billing_tn_t),"some_billing_tn_t",indent+2);
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_billing_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case billing_tn_t_err: 
      {
        // error case
        break;
      }
    }
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t billing_tn_t_write_xml_2io (P_t *pads,Sfio_t *io,billing_tn_t_pd *pd,billing_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("billing_tn_t_write_xml_2io",billing_tn_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent,order_num,att_order_num,ord_version,service_tn));
  return -1;
}
char const *billing_tn_t_tag2str (billing_tn_t_tag which)
{
  switch (which)
    {
      
    case some_billing_tn_t: 
      {
        return "some_billing_tn_t";
      }
      
    case none_billing_tn_t: 
      {
        return "none_billing_tn_t";
      }
      
    default: 
      {
        return "*unknown_tag*";
      }
    }
}
Perror_t billing_tn_t_init (P_t *pads,billing_tn_t *rep)
{
  return P_OK;
}
Perror_t billing_tn_t_pd_init (P_t *pads,billing_tn_t_pd *pd)
{
  return P_OK;
}
Perror_t billing_tn_t_cleanup (P_t *pads,billing_tn_t *rep)
{
  return P_OK;
}
Perror_t billing_tn_t_pd_cleanup (P_t *pads,billing_tn_t_pd *pd)
{
  return P_OK;
}
Perror_t billing_tn_t_copy (P_t *pads,billing_tn_t *rep_dst,billing_tn_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("billing_tn_t_copy",rep_src,rep_dst);
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(billing_tn_t));
  return P_OK;
}
Perror_t billing_tn_t_pd_copy (P_t *pads,billing_tn_t_pd *pd_dst,billing_tn_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("billing_tn_t_pd_copy",pd_src,pd_dst);
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(billing_tn_t_pd));
  return P_OK;
}
void billing_tn_t_m_init (P_t *pads,billing_tn_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(billing_tn_t_m));
}
Perror_t billing_tn_t_read (P_t *pads,billing_tn_t_m *m,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn)
{
  PDCI_IODISC_3P_CHECKS ("billing_tn_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    Puint32 none_billing_tn_t_Ptmp_;
    {
      PCGEN_UNION_READ_SETUP_STAT ("billing_tn_t_read",some_billing_tn_t,billing_tn_t_cleanup,billing_tn_t_init,billing_tn_t_copy,billing_tn_t_pd_cleanup,billing_tn_t_pd_init,billing_tn_t_pd_copy);
      // Read branch 'some_billing_tn_t'
      PCGEN_UNION_READ_STAT ("billing_tn_t_read","some_billing_tn_t",some_billing_tn_t,billing_tn_t_cleanup,billing_tn_t_init,billing_tn_t_copy,billing_tn_t_pd_cleanup,billing_tn_t_pd_init,billing_tn_t_pd_copy,pn_t_read (pads,&(m->some_billing_tn_t),&((pd->val).some_billing_tn_t),&((rep->val).some_billing_tn_t)),billing_tn_t_write_xml_2io (pads,sfstderr,pd,rep,"billing_tn_t",4,order_num,att_order_num,ord_version,service_tn),_NOOP);
      // Pcompute branch 'none_billing_tn_t'
      PCGEN_UNION_READ_MAN_STAT_VIRT_PRE ("billing_tn_t_read",none_billing_tn_t,billing_tn_t_init,billing_tn_t_pd_init);
      none_billing_tn_t_Ptmp_ = 0;
      PCGEN_UNION_READ_MAN_STAT_POST ("billing_tn_t_read",billing_tn_t_copy,billing_tn_t_cleanup,billing_tn_t_pd_copy,billing_tn_t_pd_cleanup);
      // Failed to match any branch of union billing_tn_t
      PCGEN_UNION_READ_CHECK_FAILED ("billing_tn_t_read","billing_tn_t",billing_tn_t_err);
      
    branches_done: 
      {
      }
      
    final_check: 
      {
      }
    }
    return ((pd->nerr)==0) ? P_OK : P_ERR;
  }
}
int is_billing_tn_t (billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn)
{
  int isValid=1;
  switch (rep->tag)
    {
      
    case some_billing_tn_t: 
      {
        isValid = is_pn_t (&((rep->val).some_billing_tn_t));
        break;
      }
      
    case none_billing_tn_t: 
      {
        // Pomit branch: no user constraint
        break;
      }
      
    case billing_tn_t_err: 
      {
        // error case
        {
          isValid = 0;
          break;
        }
      }
    }
  return isValid;
}
Perror_t billing_tn_t_acc_init (P_t *pads,billing_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_init (pads,&(acc->some_billing_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t billing_tn_t_acc_reset (P_t *pads,billing_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_reset (pads,&(acc->some_billing_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t billing_tn_t_acc_cleanup (P_t *pads,billing_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_cleanup (pads,&(acc->some_billing_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t billing_tn_t_acc_add (P_t *pads,billing_tn_t_acc *acc,billing_tn_t_pd *pd,billing_tn_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = (((pd->errCode)==P_UNION_MATCH_ERR) ? P_UNION_MATCH_ERR : P_NO_ERR);
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Pint32_acc_add (pads,&(acc->tag),&tpd,(Pint32 *) (&(rep->tag)))) 
        {
          nerr++;
        }
      switch (rep->tag)
        {
          
        case some_billing_tn_t: 
          {
            if (P_ERR==pn_t_acc_add (pads,&(acc->some_billing_tn_t),&((pd->val).some_billing_tn_t),&((rep->val).some_billing_tn_t))) 
              {
                nerr++;
              }
            break;
          }
          
        case none_billing_tn_t: 
          {
            // Pomit branch: cannot accumulate
            break;
          }
          
        case billing_tn_t_err: 
          {
            // error case
            break;
          }
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t billing_tn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,billing_tn_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "opt billing_tn_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_UNION_ACC_REP_NOVALS ();
  if (P_ERR==Pint32_acc_map_report2io (pads,outstr,"Opt tag","tag",-1,(Pint32_map_fn) billing_tn_t_tag2str,&(acc->tag))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each tag arm of %s]\n",prefix);
  sfprintf (tmpstr,"%s.some_billing_tn_t",prefix);
  if (P_ERR==pn_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->some_billing_tn_t))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  // Pomit branch: cannot accumulate
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t billing_tn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,billing_tn_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = billing_tn_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t billing_tn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,billing_tn_t_m *m,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("billing_tn_t_fmt2buf_final");
  switch (rep->tag)
    {
      
    case some_billing_tn_t: 
      {
        PCGEN_FMT2BUF_UNION ("pn_t_fmt2buf",pn_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->some_billing_tn_t),&((pd->val).some_billing_tn_t),&((rep->val).some_billing_tn_t)),"some_billing_tn_t");
        break;
      }
      
    case none_billing_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case billing_tn_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t billing_tn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,billing_tn_t_m *m,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("billing_tn_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"billing_tn_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn));
  return billing_tn_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn);
}
ssize_t billing_tn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,billing_tn_t_m *m,billing_tn_t_pd *pd,billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("billing_tn_t_fmt2io",billing_tn_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn));
  return -1;
}
ssize_t nlp_service_tn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("nlp_service_tn_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  switch (rep->tag)
    {
      
    case some_nlp_service_tn_t: 
      {
        tlen_PCGEN_ = pn_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_nlp_service_tn_t),&((rep->val).some_nlp_service_tn_t));
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_nlp_service_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case nlp_service_tn_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t nlp_service_tn_t_write2io (P_t *pads,Sfio_t *io,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("nlp_service_tn_t_write2io",nlp_service_tn_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn));
  return -1;
}
ssize_t nlp_service_tn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("nlp_service_tn_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("nlp_service_tn_t");
  PCGEN_UNION_PD_XML_OUT ();
  switch (rep->tag)
    {
      
    case some_nlp_service_tn_t: 
      {
        tlen_PCGEN_ = pn_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_nlp_service_tn_t),&((rep->val).some_nlp_service_tn_t),"some_nlp_service_tn_t",indent+2);
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_nlp_service_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case nlp_service_tn_t_err: 
      {
        // error case
        break;
      }
    }
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t nlp_service_tn_t_write_xml_2io (P_t *pads,Sfio_t *io,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("nlp_service_tn_t_write_xml_2io",nlp_service_tn_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent,order_num,att_order_num,ord_version,service_tn,billing_tn));
  return -1;
}
char const *nlp_service_tn_t_tag2str (nlp_service_tn_t_tag which)
{
  switch (which)
    {
      
    case some_nlp_service_tn_t: 
      {
        return "some_nlp_service_tn_t";
      }
      
    case none_nlp_service_tn_t: 
      {
        return "none_nlp_service_tn_t";
      }
      
    default: 
      {
        return "*unknown_tag*";
      }
    }
}
Perror_t nlp_service_tn_t_init (P_t *pads,nlp_service_tn_t *rep)
{
  return P_OK;
}
Perror_t nlp_service_tn_t_pd_init (P_t *pads,nlp_service_tn_t_pd *pd)
{
  return P_OK;
}
Perror_t nlp_service_tn_t_cleanup (P_t *pads,nlp_service_tn_t *rep)
{
  return P_OK;
}
Perror_t nlp_service_tn_t_pd_cleanup (P_t *pads,nlp_service_tn_t_pd *pd)
{
  return P_OK;
}
Perror_t nlp_service_tn_t_copy (P_t *pads,nlp_service_tn_t *rep_dst,nlp_service_tn_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("nlp_service_tn_t_copy",rep_src,rep_dst);
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(nlp_service_tn_t));
  return P_OK;
}
Perror_t nlp_service_tn_t_pd_copy (P_t *pads,nlp_service_tn_t_pd *pd_dst,nlp_service_tn_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("nlp_service_tn_t_pd_copy",pd_src,pd_dst);
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(nlp_service_tn_t_pd));
  return P_OK;
}
void nlp_service_tn_t_m_init (P_t *pads,nlp_service_tn_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(nlp_service_tn_t_m));
}
Perror_t nlp_service_tn_t_read (P_t *pads,nlp_service_tn_t_m *m,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn)
{
  PDCI_IODISC_3P_CHECKS ("nlp_service_tn_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    Puint32 none_nlp_service_tn_t_Ptmp_;
    {
      PCGEN_UNION_READ_SETUP_STAT ("nlp_service_tn_t_read",some_nlp_service_tn_t,nlp_service_tn_t_cleanup,nlp_service_tn_t_init,nlp_service_tn_t_copy,nlp_service_tn_t_pd_cleanup,nlp_service_tn_t_pd_init,nlp_service_tn_t_pd_copy);
      // Read branch 'some_nlp_service_tn_t'
      PCGEN_UNION_READ_STAT ("nlp_service_tn_t_read","some_nlp_service_tn_t",some_nlp_service_tn_t,nlp_service_tn_t_cleanup,nlp_service_tn_t_init,nlp_service_tn_t_copy,nlp_service_tn_t_pd_cleanup,nlp_service_tn_t_pd_init,nlp_service_tn_t_pd_copy,pn_t_read (pads,&(m->some_nlp_service_tn_t),&((pd->val).some_nlp_service_tn_t),&((rep->val).some_nlp_service_tn_t)),nlp_service_tn_t_write_xml_2io (pads,sfstderr,pd,rep,"nlp_service_tn_t",4,order_num,att_order_num,ord_version,service_tn,billing_tn),_NOOP);
      // Pcompute branch 'none_nlp_service_tn_t'
      PCGEN_UNION_READ_MAN_STAT_VIRT_PRE ("nlp_service_tn_t_read",none_nlp_service_tn_t,nlp_service_tn_t_init,nlp_service_tn_t_pd_init);
      none_nlp_service_tn_t_Ptmp_ = 0;
      PCGEN_UNION_READ_MAN_STAT_POST ("nlp_service_tn_t_read",nlp_service_tn_t_copy,nlp_service_tn_t_cleanup,nlp_service_tn_t_pd_copy,nlp_service_tn_t_pd_cleanup);
      // Failed to match any branch of union nlp_service_tn_t
      PCGEN_UNION_READ_CHECK_FAILED ("nlp_service_tn_t_read","nlp_service_tn_t",nlp_service_tn_t_err);
      
    branches_done: 
      {
      }
      
    final_check: 
      {
      }
    }
    return ((pd->nerr)==0) ? P_OK : P_ERR;
  }
}
int is_nlp_service_tn_t (nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn)
{
  int isValid=1;
  switch (rep->tag)
    {
      
    case some_nlp_service_tn_t: 
      {
        isValid = is_pn_t (&((rep->val).some_nlp_service_tn_t));
        break;
      }
      
    case none_nlp_service_tn_t: 
      {
        // Pomit branch: no user constraint
        break;
      }
      
    case nlp_service_tn_t_err: 
      {
        // error case
        {
          isValid = 0;
          break;
        }
      }
    }
  return isValid;
}
Perror_t nlp_service_tn_t_acc_init (P_t *pads,nlp_service_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_init (pads,&(acc->some_nlp_service_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t nlp_service_tn_t_acc_reset (P_t *pads,nlp_service_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_reset (pads,&(acc->some_nlp_service_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t nlp_service_tn_t_acc_cleanup (P_t *pads,nlp_service_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_cleanup (pads,&(acc->some_nlp_service_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t nlp_service_tn_t_acc_add (P_t *pads,nlp_service_tn_t_acc *acc,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = (((pd->errCode)==P_UNION_MATCH_ERR) ? P_UNION_MATCH_ERR : P_NO_ERR);
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Pint32_acc_add (pads,&(acc->tag),&tpd,(Pint32 *) (&(rep->tag)))) 
        {
          nerr++;
        }
      switch (rep->tag)
        {
          
        case some_nlp_service_tn_t: 
          {
            if (P_ERR==pn_t_acc_add (pads,&(acc->some_nlp_service_tn_t),&((pd->val).some_nlp_service_tn_t),&((rep->val).some_nlp_service_tn_t))) 
              {
                nerr++;
              }
            break;
          }
          
        case none_nlp_service_tn_t: 
          {
            // Pomit branch: cannot accumulate
            break;
          }
          
        case nlp_service_tn_t_err: 
          {
            // error case
            break;
          }
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t nlp_service_tn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,nlp_service_tn_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "opt nlp_service_tn_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_UNION_ACC_REP_NOVALS ();
  if (P_ERR==Pint32_acc_map_report2io (pads,outstr,"Opt tag","tag",-1,(Pint32_map_fn) nlp_service_tn_t_tag2str,&(acc->tag))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each tag arm of %s]\n",prefix);
  sfprintf (tmpstr,"%s.some_nlp_service_tn_t",prefix);
  if (P_ERR==pn_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->some_nlp_service_tn_t))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  // Pomit branch: cannot accumulate
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t nlp_service_tn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,nlp_service_tn_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = nlp_service_tn_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t nlp_service_tn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,nlp_service_tn_t_m *m,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("nlp_service_tn_t_fmt2buf_final");
  switch (rep->tag)
    {
      
    case some_nlp_service_tn_t: 
      {
        PCGEN_FMT2BUF_UNION ("pn_t_fmt2buf",pn_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->some_nlp_service_tn_t),&((pd->val).some_nlp_service_tn_t),&((rep->val).some_nlp_service_tn_t)),"some_nlp_service_tn_t");
        break;
      }
      
    case none_nlp_service_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case nlp_service_tn_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t nlp_service_tn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,nlp_service_tn_t_m *m,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("nlp_service_tn_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"nlp_service_tn_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn));
  return nlp_service_tn_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn);
}
ssize_t nlp_service_tn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,nlp_service_tn_t_m *m,nlp_service_tn_t_pd *pd,nlp_service_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("nlp_service_tn_t_fmt2io",nlp_service_tn_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn));
  return -1;
}
ssize_t nlp_billing_tn_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("nlp_billing_tn_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  switch (rep->tag)
    {
      
    case some_nlp_billing_tn_t: 
      {
        tlen_PCGEN_ = pn_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_nlp_billing_tn_t),&((rep->val).some_nlp_billing_tn_t));
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_nlp_billing_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case nlp_billing_tn_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t nlp_billing_tn_t_write2io (P_t *pads,Sfio_t *io,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("nlp_billing_tn_t_write2io",nlp_billing_tn_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn));
  return -1;
}
ssize_t nlp_billing_tn_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("nlp_billing_tn_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("nlp_billing_tn_t");
  PCGEN_UNION_PD_XML_OUT ();
  switch (rep->tag)
    {
      
    case some_nlp_billing_tn_t: 
      {
        tlen_PCGEN_ = pn_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_nlp_billing_tn_t),&((rep->val).some_nlp_billing_tn_t),"some_nlp_billing_tn_t",indent+2);
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_nlp_billing_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case nlp_billing_tn_t_err: 
      {
        // error case
        break;
      }
    }
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t nlp_billing_tn_t_write_xml_2io (P_t *pads,Sfio_t *io,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("nlp_billing_tn_t_write_xml_2io",nlp_billing_tn_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn));
  return -1;
}
char const *nlp_billing_tn_t_tag2str (nlp_billing_tn_t_tag which)
{
  switch (which)
    {
      
    case some_nlp_billing_tn_t: 
      {
        return "some_nlp_billing_tn_t";
      }
      
    case none_nlp_billing_tn_t: 
      {
        return "none_nlp_billing_tn_t";
      }
      
    default: 
      {
        return "*unknown_tag*";
      }
    }
}
Perror_t nlp_billing_tn_t_init (P_t *pads,nlp_billing_tn_t *rep)
{
  return P_OK;
}
Perror_t nlp_billing_tn_t_pd_init (P_t *pads,nlp_billing_tn_t_pd *pd)
{
  return P_OK;
}
Perror_t nlp_billing_tn_t_cleanup (P_t *pads,nlp_billing_tn_t *rep)
{
  return P_OK;
}
Perror_t nlp_billing_tn_t_pd_cleanup (P_t *pads,nlp_billing_tn_t_pd *pd)
{
  return P_OK;
}
Perror_t nlp_billing_tn_t_copy (P_t *pads,nlp_billing_tn_t *rep_dst,nlp_billing_tn_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("nlp_billing_tn_t_copy",rep_src,rep_dst);
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(nlp_billing_tn_t));
  return P_OK;
}
Perror_t nlp_billing_tn_t_pd_copy (P_t *pads,nlp_billing_tn_t_pd *pd_dst,nlp_billing_tn_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("nlp_billing_tn_t_pd_copy",pd_src,pd_dst);
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(nlp_billing_tn_t_pd));
  return P_OK;
}
void nlp_billing_tn_t_m_init (P_t *pads,nlp_billing_tn_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(nlp_billing_tn_t_m));
}
Perror_t nlp_billing_tn_t_read (P_t *pads,nlp_billing_tn_t_m *m,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn)
{
  PDCI_IODISC_3P_CHECKS ("nlp_billing_tn_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    Puint32 none_nlp_billing_tn_t_Ptmp_;
    {
      PCGEN_UNION_READ_SETUP_STAT ("nlp_billing_tn_t_read",some_nlp_billing_tn_t,nlp_billing_tn_t_cleanup,nlp_billing_tn_t_init,nlp_billing_tn_t_copy,nlp_billing_tn_t_pd_cleanup,nlp_billing_tn_t_pd_init,nlp_billing_tn_t_pd_copy);
      // Read branch 'some_nlp_billing_tn_t'
      PCGEN_UNION_READ_STAT ("nlp_billing_tn_t_read","some_nlp_billing_tn_t",some_nlp_billing_tn_t,nlp_billing_tn_t_cleanup,nlp_billing_tn_t_init,nlp_billing_tn_t_copy,nlp_billing_tn_t_pd_cleanup,nlp_billing_tn_t_pd_init,nlp_billing_tn_t_pd_copy,pn_t_read (pads,&(m->some_nlp_billing_tn_t),&((pd->val).some_nlp_billing_tn_t),&((rep->val).some_nlp_billing_tn_t)),nlp_billing_tn_t_write_xml_2io (pads,sfstderr,pd,rep,"nlp_billing_tn_t",4,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn),_NOOP);
      // Pcompute branch 'none_nlp_billing_tn_t'
      PCGEN_UNION_READ_MAN_STAT_VIRT_PRE ("nlp_billing_tn_t_read",none_nlp_billing_tn_t,nlp_billing_tn_t_init,nlp_billing_tn_t_pd_init);
      none_nlp_billing_tn_t_Ptmp_ = 0;
      PCGEN_UNION_READ_MAN_STAT_POST ("nlp_billing_tn_t_read",nlp_billing_tn_t_copy,nlp_billing_tn_t_cleanup,nlp_billing_tn_t_pd_copy,nlp_billing_tn_t_pd_cleanup);
      // Failed to match any branch of union nlp_billing_tn_t
      PCGEN_UNION_READ_CHECK_FAILED ("nlp_billing_tn_t_read","nlp_billing_tn_t",nlp_billing_tn_t_err);
      
    branches_done: 
      {
      }
      
    final_check: 
      {
      }
    }
    return ((pd->nerr)==0) ? P_OK : P_ERR;
  }
}
int is_nlp_billing_tn_t (nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn)
{
  int isValid=1;
  switch (rep->tag)
    {
      
    case some_nlp_billing_tn_t: 
      {
        isValid = is_pn_t (&((rep->val).some_nlp_billing_tn_t));
        break;
      }
      
    case none_nlp_billing_tn_t: 
      {
        // Pomit branch: no user constraint
        break;
      }
      
    case nlp_billing_tn_t_err: 
      {
        // error case
        {
          isValid = 0;
          break;
        }
      }
    }
  return isValid;
}
Perror_t nlp_billing_tn_t_acc_init (P_t *pads,nlp_billing_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_init (pads,&(acc->some_nlp_billing_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t nlp_billing_tn_t_acc_reset (P_t *pads,nlp_billing_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_reset (pads,&(acc->some_nlp_billing_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t nlp_billing_tn_t_acc_cleanup (P_t *pads,nlp_billing_tn_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==pn_t_acc_cleanup (pads,&(acc->some_nlp_billing_tn_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t nlp_billing_tn_t_acc_add (P_t *pads,nlp_billing_tn_t_acc *acc,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = (((pd->errCode)==P_UNION_MATCH_ERR) ? P_UNION_MATCH_ERR : P_NO_ERR);
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Pint32_acc_add (pads,&(acc->tag),&tpd,(Pint32 *) (&(rep->tag)))) 
        {
          nerr++;
        }
      switch (rep->tag)
        {
          
        case some_nlp_billing_tn_t: 
          {
            if (P_ERR==pn_t_acc_add (pads,&(acc->some_nlp_billing_tn_t),&((pd->val).some_nlp_billing_tn_t),&((rep->val).some_nlp_billing_tn_t))) 
              {
                nerr++;
              }
            break;
          }
          
        case none_nlp_billing_tn_t: 
          {
            // Pomit branch: cannot accumulate
            break;
          }
          
        case nlp_billing_tn_t_err: 
          {
            // error case
            break;
          }
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t nlp_billing_tn_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,nlp_billing_tn_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "opt nlp_billing_tn_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_UNION_ACC_REP_NOVALS ();
  if (P_ERR==Pint32_acc_map_report2io (pads,outstr,"Opt tag","tag",-1,(Pint32_map_fn) nlp_billing_tn_t_tag2str,&(acc->tag))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each tag arm of %s]\n",prefix);
  sfprintf (tmpstr,"%s.some_nlp_billing_tn_t",prefix);
  if (P_ERR==pn_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->some_nlp_billing_tn_t))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  // Pomit branch: cannot accumulate
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t nlp_billing_tn_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,nlp_billing_tn_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = nlp_billing_tn_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t nlp_billing_tn_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,nlp_billing_tn_t_m *m,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("nlp_billing_tn_t_fmt2buf_final");
  switch (rep->tag)
    {
      
    case some_nlp_billing_tn_t: 
      {
        PCGEN_FMT2BUF_UNION ("pn_t_fmt2buf",pn_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->some_nlp_billing_tn_t),&((pd->val).some_nlp_billing_tn_t),&((rep->val).some_nlp_billing_tn_t)),"some_nlp_billing_tn_t");
        break;
      }
      
    case none_nlp_billing_tn_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case nlp_billing_tn_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t nlp_billing_tn_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,nlp_billing_tn_t_m *m,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("nlp_billing_tn_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"nlp_billing_tn_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn));
  return nlp_billing_tn_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn);
}
ssize_t nlp_billing_tn_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,nlp_billing_tn_t_m *m,nlp_billing_tn_t_pd *pd,nlp_billing_tn_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("nlp_billing_tn_t_fmt2io",nlp_billing_tn_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn));
  return -1;
}
ssize_t zip_code_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("zip_code_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  switch (rep->tag)
    {
      
    case some_zip_code_t: 
      {
        tlen_PCGEN_ = zip_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_zip_code_t),&((rep->val).some_zip_code_t));
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_zip_code_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case zip_code_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t zip_code_t_write2io (P_t *pads,Sfio_t *io,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("zip_code_t_write2io",zip_code_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn,nlp_billing_tn));
  return -1;
}
ssize_t zip_code_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,zip_code_t_pd *pd,zip_code_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("zip_code_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("zip_code_t");
  PCGEN_UNION_PD_XML_OUT ();
  switch (rep->tag)
    {
      
    case some_zip_code_t: 
      {
        tlen_PCGEN_ = zip_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&((pd->val).some_zip_code_t),&((rep->val).some_zip_code_t),"some_zip_code_t",indent+2);
        PCGEN_TLEN_UPDATES ();
        break;
      }
      
    case none_zip_code_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case zip_code_t_err: 
      {
        // error case
        break;
      }
    }
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t zip_code_t_write_xml_2io (P_t *pads,Sfio_t *io,zip_code_t_pd *pd,zip_code_t *rep,char const *tag,int indent,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("zip_code_t_write_xml_2io",zip_code_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn,nlp_billing_tn));
  return -1;
}
char const *zip_code_t_tag2str (zip_code_t_tag which)
{
  switch (which)
    {
      
    case some_zip_code_t: 
      {
        return "some_zip_code_t";
      }
      
    case none_zip_code_t: 
      {
        return "none_zip_code_t";
      }
      
    default: 
      {
        return "*unknown_tag*";
      }
    }
}
Perror_t zip_code_t_init (P_t *pads,zip_code_t *rep)
{
  return P_OK;
}
Perror_t zip_code_t_pd_init (P_t *pads,zip_code_t_pd *pd)
{
  return P_OK;
}
Perror_t zip_code_t_cleanup (P_t *pads,zip_code_t *rep)
{
  return P_OK;
}
Perror_t zip_code_t_pd_cleanup (P_t *pads,zip_code_t_pd *pd)
{
  return P_OK;
}
Perror_t zip_code_t_copy (P_t *pads,zip_code_t *rep_dst,zip_code_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("zip_code_t_copy",rep_src,rep_dst);
  memcpy ((void *) rep_dst,(void *) rep_src,sizeof(zip_code_t));
  return P_OK;
}
Perror_t zip_code_t_pd_copy (P_t *pads,zip_code_t_pd *pd_dst,zip_code_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("zip_code_t_pd_copy",pd_src,pd_dst);
  memcpy ((void *) pd_dst,(void *) pd_src,sizeof(zip_code_t_pd));
  return P_OK;
}
void zip_code_t_m_init (P_t *pads,zip_code_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(zip_code_t_m));
}
Perror_t zip_code_t_read (P_t *pads,zip_code_t_m *m,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn)
{
  PDCI_IODISC_3P_CHECKS ("zip_code_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    Puint32 none_zip_code_t_Ptmp_;
    {
      PCGEN_UNION_READ_SETUP_STAT ("zip_code_t_read",some_zip_code_t,zip_code_t_cleanup,zip_code_t_init,zip_code_t_copy,zip_code_t_pd_cleanup,zip_code_t_pd_init,zip_code_t_pd_copy);
      // Read branch 'some_zip_code_t'
      PCGEN_UNION_READ_STAT ("zip_code_t_read","some_zip_code_t",some_zip_code_t,zip_code_t_cleanup,zip_code_t_init,zip_code_t_copy,zip_code_t_pd_cleanup,zip_code_t_pd_init,zip_code_t_pd_copy,zip_t_read (pads,&(m->some_zip_code_t),&((pd->val).some_zip_code_t),&((rep->val).some_zip_code_t)),zip_code_t_write_xml_2io (pads,sfstderr,pd,rep,"zip_code_t",4,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn,nlp_billing_tn),_NOOP);
      // Pcompute branch 'none_zip_code_t'
      PCGEN_UNION_READ_MAN_STAT_VIRT_PRE ("zip_code_t_read",none_zip_code_t,zip_code_t_init,zip_code_t_pd_init);
      none_zip_code_t_Ptmp_ = 0;
      PCGEN_UNION_READ_MAN_STAT_POST ("zip_code_t_read",zip_code_t_copy,zip_code_t_cleanup,zip_code_t_pd_copy,zip_code_t_pd_cleanup);
      // Failed to match any branch of union zip_code_t
      PCGEN_UNION_READ_CHECK_FAILED ("zip_code_t_read","zip_code_t",zip_code_t_err);
      
    branches_done: 
      {
      }
      
    final_check: 
      {
      }
    }
    return ((pd->nerr)==0) ? P_OK : P_ERR;
  }
}
int is_zip_code_t (zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn)
{
  int isValid=1;
  switch (rep->tag)
    {
      
    case some_zip_code_t: 
      {
        isValid = is_zip_t (&((rep->val).some_zip_code_t));
        break;
      }
      
    case none_zip_code_t: 
      {
        // Pomit branch: no user constraint
        break;
      }
      
    case zip_code_t_err: 
      {
        // error case
        {
          isValid = 0;
          break;
        }
      }
    }
  return isValid;
}
Perror_t zip_code_t_acc_init (P_t *pads,zip_code_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==zip_t_acc_init (pads,&(acc->some_zip_code_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t zip_code_t_acc_reset (P_t *pads,zip_code_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==zip_t_acc_reset (pads,&(acc->some_zip_code_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t zip_code_t_acc_cleanup (P_t *pads,zip_code_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==zip_t_acc_cleanup (pads,&(acc->some_zip_code_t))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t zip_code_t_acc_add (P_t *pads,zip_code_t_acc *acc,zip_code_t_pd *pd,zip_code_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = (((pd->errCode)==P_UNION_MATCH_ERR) ? P_UNION_MATCH_ERR : P_NO_ERR);
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Pint32_acc_add (pads,&(acc->tag),&tpd,(Pint32 *) (&(rep->tag)))) 
        {
          nerr++;
        }
      switch (rep->tag)
        {
          
        case some_zip_code_t: 
          {
            if (P_ERR==zip_t_acc_add (pads,&(acc->some_zip_code_t),&((pd->val).some_zip_code_t),&((rep->val).some_zip_code_t))) 
              {
                nerr++;
              }
            break;
          }
          
        case none_zip_code_t: 
          {
            // Pomit branch: cannot accumulate
            break;
          }
          
        case zip_code_t_err: 
          {
            // error case
            break;
          }
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t zip_code_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,zip_code_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "opt zip_code_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_UNION_ACC_REP_NOVALS ();
  if (P_ERR==Pint32_acc_map_report2io (pads,outstr,"Opt tag","tag",-1,(Pint32_map_fn) zip_code_t_tag2str,&(acc->tag))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each tag arm of %s]\n",prefix);
  sfprintf (tmpstr,"%s.some_zip_code_t",prefix);
  if (P_ERR==zip_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->some_zip_code_t))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  // Pomit branch: cannot accumulate
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t zip_code_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,zip_code_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = zip_code_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t zip_code_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zip_code_t_m *m,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("zip_code_t_fmt2buf_final");
  switch (rep->tag)
    {
      
    case some_zip_code_t: 
      {
        PCGEN_FMT2BUF_UNION ("zip_t_fmt2buf",zip_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->some_zip_code_t),&((pd->val).some_zip_code_t),&((rep->val).some_zip_code_t)),"some_zip_code_t");
        break;
      }
      
    case none_zip_code_t: 
      {
        // Pomit branch: cannot output
        break;
      }
      
    case zip_code_t_err: 
      {
        // error case
        break;
      }
    }
  return length_PCGEN_;
}
ssize_t zip_code_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,zip_code_t_m *m,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("zip_code_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"zip_code_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn,nlp_billing_tn));
  return zip_code_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn,nlp_billing_tn);
}
ssize_t zip_code_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,zip_code_t_m *m,zip_code_t_pd *pd,zip_code_t *rep,Puint32 *order_num,Puint32 *att_order_num,Puint32 *ord_version,service_tn_t *service_tn,billing_tn_t *billing_tn,nlp_service_tn_t *nlp_service_tn,nlp_billing_tn_t *nlp_billing_tn)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("zip_code_t_fmt2io",zip_code_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep,order_num,att_order_num,ord_version,service_tn,billing_tn,nlp_service_tn,nlp_billing_tn));
  return -1;
}
Perror_t order_header_t_init (P_t *pads,order_header_t *rep)
{
  PDCI_DISC_1P_CHECKS ("order_header_t_init",rep);
  memset ((void *) rep,0,sizeof(order_header_t));
  return P_OK;
}
Perror_t order_header_t_pd_init (P_t *pads,order_header_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("order_header_t_pd_init",pd);
  memset ((void *) pd,0,sizeof(order_header_t_pd));
  return P_OK;
}
Perror_t order_header_t_cleanup (P_t *pads,order_header_t *rep)
{
  PDCI_DISC_1P_CHECKS ("order_header_t_cleanup",rep);
  Pstring_cleanup (pads,&(rep->order_type));
  Pstring_cleanup (pads,&(rep->unused));
  Pstring_cleanup (pads,&(rep->stream));
  return P_OK;
}
Perror_t order_header_t_pd_cleanup (P_t *pads,order_header_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("order_header_t_pd_cleanup",pd);
  Pstring_pd_cleanup (pads,&(pd->order_type));
  Pstring_pd_cleanup (pads,&(pd->unused));
  Pstring_pd_cleanup (pads,&(pd->stream));
  return P_OK;
}
Perror_t order_header_t_copy (P_t *pads,order_header_t *rep_dst,order_header_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("order_header_t_copy",rep_src,rep_dst);
  // Copy fields order_num, att_order_num, ord_version, service_tn, billing_tn, nlp_service_tn, nlp_billing_tn, zip_code, ramp
  memcpy ((void *) (&(rep_dst->order_num)),(void *) (&(rep_src->order_num)),((((((((sizeof(Puint32))+(sizeof(Puint32)))+(sizeof(Puint32)))+(sizeof(service_tn_t)))+(sizeof(billing_tn_t)))+(sizeof(nlp_service_tn_t)))+(sizeof(nlp_billing_tn_t)))+(sizeof(zip_code_t)))+(sizeof(dib_ramp_t)));
  Pstring_copy (pads,&(rep_dst->order_type),&(rep_src->order_type));
  memcpy ((void *) (&(rep_dst->order_details)),(void *) (&(rep_src->order_details)),sizeof(Puint32));
  Pstring_copy (pads,&(rep_dst->unused),&(rep_src->unused));
  Pstring_copy (pads,&(rep_dst->stream),&(rep_src->stream));
  return P_OK;
}
Perror_t order_header_t_pd_copy (P_t *pads,order_header_t_pd *pd_dst,order_header_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("order_header_t_pd_copy",pd_src,pd_dst);
  // Copy fields order_num, att_order_num, ord_version, service_tn, billing_tn, nlp_service_tn, nlp_billing_tn, zip_code, ramp
  memcpy ((void *) (&(pd_dst->order_num)),(void *) (&(pd_src->order_num)),((((((((sizeof(Pbase_pd))+(sizeof(Pbase_pd)))+(sizeof(Pbase_pd)))+(sizeof(service_tn_t_pd)))+(sizeof(billing_tn_t_pd)))+(sizeof(nlp_service_tn_t_pd)))+(sizeof(nlp_billing_tn_t_pd)))+(sizeof(zip_code_t_pd)))+(sizeof(dib_ramp_t_pd)));
  Pstring_pd_copy (pads,&(pd_dst->order_type),&(pd_src->order_type));
  memcpy ((void *) (&(pd_dst->order_details)),(void *) (&(pd_src->order_details)),sizeof(Pbase_pd));
  Pstring_pd_copy (pads,&(pd_dst->unused),&(pd_src->unused));
  Pstring_pd_copy (pads,&(pd_dst->stream),&(pd_src->stream));
  return P_OK;
}
void order_header_t_m_init (P_t *pads,order_header_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(order_header_t_m));
}
Perror_t order_header_t_read (P_t *pads,order_header_t_m *m,order_header_t_pd *pd,order_header_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("order_header_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  // Read field 'order_num'
  PCGEN_STRUCT_READ_FIRST ("order_header_t_read",order_num,Puint32_read (pads,&(m->order_num),&(pd->order_num),&(rep->order_num)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'att_order_num'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",att_order_num,Puint32_read (pads,&(m->att_order_num),&(pd->att_order_num),&(rep->att_order_num)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'ord_version'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",ord_version,Puint32_read (pads,&(m->ord_version),&(pd->ord_version),&(rep->ord_version)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'service_tn'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",service_tn,service_tn_t_read (pads,&(m->service_tn),&(pd->service_tn),&(rep->service_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'billing_tn'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",billing_tn,billing_tn_t_read (pads,&(m->billing_tn),&(pd->billing_tn),&(rep->billing_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'nlp_service_tn'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",nlp_service_tn,nlp_service_tn_t_read (pads,&(m->nlp_service_tn),&(pd->nlp_service_tn),&(rep->nlp_service_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'nlp_billing_tn'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",nlp_billing_tn,nlp_billing_tn_t_read (pads,&(m->nlp_billing_tn),&(pd->nlp_billing_tn),&(rep->nlp_billing_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'zip_code'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",zip_code,zip_code_t_read (pads,&(m->zip_code),&(pd->zip_code),&(rep->zip_code),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn),&(rep->nlp_billing_tn)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'ramp'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",ramp,dib_ramp_t_read (pads,&(m->ramp),&(pd->ramp),&(rep->ramp)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'order_type'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",order_type,Pstring_read (pads,&(m->order_type),&(pd->order_type),&(rep->order_type),124),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'order_details'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",order_details,Puint32_read (pads,&(m->order_details),&(pd->order_details),&(rep->order_details)),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'unused'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",unused,Pstring_read (pads,&(m->unused),&(pd->unused),&(rep->unused),124),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  // Read field 'stream'
  PCGEN_STRUCT_READ_NEXT ("order_header_t_read",stream,Pstring_read (pads,&(m->stream),&(pd->stream),&(rep->stream),124),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("order_header_t_read",124);
  return ((pd->nerr)==0) ? P_OK : P_ERR;
}
int is_order_header_t (order_header_t *rep)
{
  return is_service_tn_t (&(rep->service_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version))&&(is_billing_tn_t (&(rep->billing_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn))&&(is_nlp_service_tn_t (&(rep->nlp_service_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn))&&(is_nlp_billing_tn_t (&(rep->nlp_billing_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn))&&(is_zip_code_t (&(rep->zip_code),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn),&(rep->nlp_billing_tn))&&(is_dib_ramp_t (&(rep->ramp))&&1)))));
}
Perror_t order_header_t_acc_init (P_t *pads,order_header_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_init (pads,&(acc->order_num))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_init (pads,&(acc->att_order_num))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_init (pads,&(acc->ord_version))) 
    {
      nerr++;
    }
  if (P_ERR==service_tn_t_acc_init (pads,&(acc->service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==billing_tn_t_acc_init (pads,&(acc->billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==nlp_service_tn_t_acc_init (pads,&(acc->nlp_service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==nlp_billing_tn_t_acc_init (pads,&(acc->nlp_billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==zip_code_t_acc_init (pads,&(acc->zip_code))) 
    {
      nerr++;
    }
  if (P_ERR==dib_ramp_t_acc_init (pads,&(acc->ramp))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_init (pads,&(acc->order_type))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_init (pads,&(acc->order_details))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_init (pads,&(acc->unused))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_init (pads,&(acc->stream))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t order_header_t_acc_reset (P_t *pads,order_header_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_reset (pads,&(acc->order_num))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_reset (pads,&(acc->att_order_num))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_reset (pads,&(acc->ord_version))) 
    {
      nerr++;
    }
  if (P_ERR==service_tn_t_acc_reset (pads,&(acc->service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==billing_tn_t_acc_reset (pads,&(acc->billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==nlp_service_tn_t_acc_reset (pads,&(acc->nlp_service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==nlp_billing_tn_t_acc_reset (pads,&(acc->nlp_billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==zip_code_t_acc_reset (pads,&(acc->zip_code))) 
    {
      nerr++;
    }
  if (P_ERR==dib_ramp_t_acc_reset (pads,&(acc->ramp))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_reset (pads,&(acc->order_type))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_reset (pads,&(acc->order_details))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_reset (pads,&(acc->unused))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_reset (pads,&(acc->stream))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t order_header_t_acc_cleanup (P_t *pads,order_header_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->order_num))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->att_order_num))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->ord_version))) 
    {
      nerr++;
    }
  if (P_ERR==service_tn_t_acc_cleanup (pads,&(acc->service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==billing_tn_t_acc_cleanup (pads,&(acc->billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==nlp_service_tn_t_acc_cleanup (pads,&(acc->nlp_service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==nlp_billing_tn_t_acc_cleanup (pads,&(acc->nlp_billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==zip_code_t_acc_cleanup (pads,&(acc->zip_code))) 
    {
      nerr++;
    }
  if (P_ERR==dib_ramp_t_acc_cleanup (pads,&(acc->ramp))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_cleanup (pads,&(acc->order_type))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->order_details))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_cleanup (pads,&(acc->unused))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_cleanup (pads,&(acc->stream))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t order_header_t_acc_add (P_t *pads,order_header_t_acc *acc,order_header_t_pd *pd,order_header_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = P_NO_ERR;
  if (P_ERR==Puint32_acc_add (pads,&(acc->nerr),&tpd,&(pd->nerr))) 
    {
      nerr++;
    }
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Puint32_acc_add (pads,&(acc->order_num),&(pd->order_num),&(rep->order_num))) 
        {
          nerr++;
        }
      if (P_ERR==Puint32_acc_add (pads,&(acc->att_order_num),&(pd->att_order_num),&(rep->att_order_num))) 
        {
          nerr++;
        }
      if (P_ERR==Puint32_acc_add (pads,&(acc->ord_version),&(pd->ord_version),&(rep->ord_version))) 
        {
          nerr++;
        }
      if (P_ERR==service_tn_t_acc_add (pads,&(acc->service_tn),&(pd->service_tn),&(rep->service_tn))) 
        {
          nerr++;
        }
      if (P_ERR==billing_tn_t_acc_add (pads,&(acc->billing_tn),&(pd->billing_tn),&(rep->billing_tn))) 
        {
          nerr++;
        }
      if (P_ERR==nlp_service_tn_t_acc_add (pads,&(acc->nlp_service_tn),&(pd->nlp_service_tn),&(rep->nlp_service_tn))) 
        {
          nerr++;
        }
      if (P_ERR==nlp_billing_tn_t_acc_add (pads,&(acc->nlp_billing_tn),&(pd->nlp_billing_tn),&(rep->nlp_billing_tn))) 
        {
          nerr++;
        }
      if (P_ERR==zip_code_t_acc_add (pads,&(acc->zip_code),&(pd->zip_code),&(rep->zip_code))) 
        {
          nerr++;
        }
      if (P_ERR==dib_ramp_t_acc_add (pads,&(acc->ramp),&(pd->ramp),&(rep->ramp))) 
        {
          nerr++;
        }
      if (P_ERR==Pstring_acc_add (pads,&(acc->order_type),&(pd->order_type),&(rep->order_type))) 
        {
          nerr++;
        }
      if (P_ERR==Puint32_acc_add (pads,&(acc->order_details),&(pd->order_details),&(rep->order_details))) 
        {
          nerr++;
        }
      if (P_ERR==Pstring_acc_add (pads,&(acc->unused),&(pd->unused),&(rep->unused))) 
        {
          nerr++;
        }
      if (P_ERR==Pstring_acc_add (pads,&(acc->stream),&(pd->stream),&(rep->stream))) 
        {
          nerr++;
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t order_header_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,order_header_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "struct order_header_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_STRUCT_ACC_REP_NOVALS ();
  if (P_ERR==P_nerr_acc_report2io (pads,outstr,"Errors","errors",-1,&(acc->nerr))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.order_num",prefix);
  if (P_ERR==Puint32_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->order_num))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.att_order_num",prefix);
  if (P_ERR==Puint32_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->att_order_num))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.ord_version",prefix);
  if (P_ERR==Puint32_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->ord_version))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.service_tn",prefix);
  if (P_ERR==service_tn_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->service_tn))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.billing_tn",prefix);
  if (P_ERR==billing_tn_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->billing_tn))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.nlp_service_tn",prefix);
  if (P_ERR==nlp_service_tn_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->nlp_service_tn))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.nlp_billing_tn",prefix);
  if (P_ERR==nlp_billing_tn_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->nlp_billing_tn))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.zip_code",prefix);
  if (P_ERR==zip_code_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->zip_code))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.ramp",prefix);
  if (P_ERR==dib_ramp_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->ramp))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.order_type",prefix);
  if (P_ERR==Pstring_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->order_type))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.order_details",prefix);
  if (P_ERR==Puint32_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->order_details))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.unused",prefix);
  if (P_ERR==Pstring_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->unused))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.stream",prefix);
  if (P_ERR==Pstring_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->stream))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t order_header_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,order_header_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = order_header_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t order_header_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,order_header_t_pd *pd,order_header_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("order_header_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  tlen_PCGEN_ = Puint32_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->order_num),&(rep->order_num));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint32_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->att_order_num),&(rep->att_order_num));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint32_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->ord_version),&(rep->ord_version));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = service_tn_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->service_tn),&(rep->service_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = billing_tn_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->billing_tn),&(rep->billing_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = nlp_service_tn_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->nlp_service_tn),&(rep->nlp_service_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = nlp_billing_tn_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->nlp_billing_tn),&(rep->nlp_billing_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = zip_code_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->zip_code),&(rep->zip_code),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn),&(rep->nlp_billing_tn));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = dib_ramp_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->ramp),&(rep->ramp));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pstring_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->order_type),&(rep->order_type),124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint32_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->order_details),&(rep->order_details));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pstring_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->unused),&(rep->unused),124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pstring_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->stream),&(rep->stream),124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t order_header_t_write2io (P_t *pads,Sfio_t *io,order_header_t_pd *pd,order_header_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("order_header_t_write2io",order_header_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t order_header_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,order_header_t_pd *pd,order_header_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("order_header_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("order_header_t");
  PCGEN_STRUCT_PD_XML_OUT ();
  tlen_PCGEN_ = Puint32_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->order_num),&(rep->order_num),"order_num",indent+2);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint32_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->att_order_num),&(rep->att_order_num),"att_order_num",indent+2);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint32_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->ord_version),&(rep->ord_version),"ord_version",indent+2);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = service_tn_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->service_tn),&(rep->service_tn),"service_tn",indent+2,&(rep->order_num),&(rep->att_order_num),&(rep->ord_version));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = billing_tn_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->billing_tn),&(rep->billing_tn),"billing_tn",indent+2,&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = nlp_service_tn_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->nlp_service_tn),&(rep->nlp_service_tn),"nlp_service_tn",indent+2,&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = nlp_billing_tn_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->nlp_billing_tn),&(rep->nlp_billing_tn),"nlp_billing_tn",indent+2,&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = zip_code_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->zip_code),&(rep->zip_code),"zip_code",indent+2,&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn),&(rep->nlp_billing_tn));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = dib_ramp_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->ramp),&(rep->ramp),"ramp",indent+2);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pstring_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->order_type),&(rep->order_type),"order_type",indent+2,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint32_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->order_details),&(rep->order_details),"order_details",indent+2);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pstring_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->unused),&(rep->unused),"unused",indent+2,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pstring_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->stream),&(rep->stream),"stream",indent+2,124);
  PCGEN_TLEN_UPDATES ();
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t order_header_t_write_xml_2io (P_t *pads,Sfio_t *io,order_header_t_pd *pd,order_header_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("order_header_t_write_xml_2io",order_header_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t order_header_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,order_header_t_m *m,order_header_t_pd *pd,order_header_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("order_header_t_fmt2buf_final");
  PCGEN_FMT2BUF_STRUCT_FIELD ("Puint32_fmt2buf",Puint32_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->order_num),&(pd->order_num),&(rep->order_num)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("Puint32_fmt2buf",Puint32_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->att_order_num),&(pd->att_order_num),&(rep->att_order_num)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("Puint32_fmt2buf",Puint32_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->ord_version),&(pd->ord_version),&(rep->ord_version)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("service_tn_t_fmt2buf",service_tn_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->service_tn),&(pd->service_tn),&(rep->service_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("billing_tn_t_fmt2buf",billing_tn_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->billing_tn),&(pd->billing_tn),&(rep->billing_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("nlp_service_tn_t_fmt2buf",nlp_service_tn_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->nlp_service_tn),&(pd->nlp_service_tn),&(rep->nlp_service_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("nlp_billing_tn_t_fmt2buf",nlp_billing_tn_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->nlp_billing_tn),&(pd->nlp_billing_tn),&(rep->nlp_billing_tn),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("zip_code_t_fmt2buf",zip_code_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->zip_code),&(pd->zip_code),&(rep->zip_code),&(rep->order_num),&(rep->att_order_num),&(rep->ord_version),&(rep->service_tn),&(rep->billing_tn),&(rep->nlp_service_tn),&(rep->nlp_billing_tn)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("dib_ramp_t_fmt2buf",dib_ramp_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->ramp),&(pd->ramp),&(rep->ramp)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("Pstring_fmt2buf",Pstring_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->order_type),&(pd->order_type),&(rep->order_type),124));
  PCGEN_FMT2BUF_STRUCT_FIELD ("Puint32_fmt2buf",Puint32_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->order_details),&(pd->order_details),&(rep->order_details)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("Pstring_fmt2buf",Pstring_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->unused),&(pd->unused),&(rep->unused),124));
  PCGEN_FMT2BUF_STRUCT_FIELD ("Pstring_fmt2buf",Pstring_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->stream),&(pd->stream),&(rep->stream),124));
  PCGEN_FMT2BUF_FIX_LAST ();
  return length_PCGEN_;
}
ssize_t order_header_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,order_header_t_m *m,order_header_t_pd *pd,order_header_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("order_header_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"order_header_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return order_header_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t order_header_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,order_header_t_m *m,order_header_t_pd *pd,order_header_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("order_header_t_fmt2io",order_header_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
Perror_t event_t_init (P_t *pads,event_t *rep)
{
  PDCI_DISC_1P_CHECKS ("event_t_init",rep);
  memset ((void *) rep,0,sizeof(event_t));
  return P_OK;
}
Perror_t event_t_pd_init (P_t *pads,event_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("event_t_pd_init",pd);
  memset ((void *) pd,0,sizeof(event_t_pd));
  return P_OK;
}
Perror_t event_t_cleanup (P_t *pads,event_t *rep)
{
  PDCI_DISC_1P_CHECKS ("event_t_cleanup",rep);
  Pstring_cleanup (pads,&(rep->state));
  return P_OK;
}
Perror_t event_t_pd_cleanup (P_t *pads,event_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("event_t_pd_cleanup",pd);
  Pstring_pd_cleanup (pads,&(pd->state));
  return P_OK;
}
Perror_t event_t_copy (P_t *pads,event_t *rep_dst,event_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("event_t_copy",rep_src,rep_dst);
  Pstring_copy (pads,&(rep_dst->state),&(rep_src->state));
  memcpy ((void *) (&(rep_dst->tstamp)),(void *) (&(rep_src->tstamp)),sizeof(Puint32));
  return P_OK;
}
Perror_t event_t_pd_copy (P_t *pads,event_t_pd *pd_dst,event_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("event_t_pd_copy",pd_src,pd_dst);
  Pstring_pd_copy (pads,&(pd_dst->state),&(pd_src->state));
  memcpy ((void *) (&(pd_dst->tstamp)),(void *) (&(pd_src->tstamp)),sizeof(Pbase_pd));
  return P_OK;
}
void event_t_m_init (P_t *pads,event_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(event_t_m));
}
Perror_t event_t_read (P_t *pads,event_t_m *m,event_t_pd *pd,event_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("event_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  // Read field 'state'
  PCGEN_STRUCT_READ_FIRST ("event_t_read",state,Pstring_read (pads,&(m->state),&(pd->state),&(rep->state),124),_NOOP);
  // Read delimter field: '|'
  PCGEN_STRUCT_READ_NEXT_CHAR_LIT ("event_t_read",124);
  // Read field 'tstamp'
  PCGEN_STRUCT_READ_NEXT ("event_t_read",tstamp,Puint32_read (pads,&(m->tstamp),&(pd->tstamp),&(rep->tstamp)),_NOOP);
  return ((pd->nerr)==0) ? P_OK : P_ERR;
}
int is_event_t (event_t *rep)
{
  return 1;
}
Perror_t event_t_acc_init (P_t *pads,event_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_init (pads,&(acc->state))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_init (pads,&(acc->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t event_t_acc_reset (P_t *pads,event_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_reset (pads,&(acc->state))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_reset (pads,&(acc->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t event_t_acc_cleanup (P_t *pads,event_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==Pstring_acc_cleanup (pads,&(acc->state))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t event_t_acc_add (P_t *pads,event_t_acc *acc,event_t_pd *pd,event_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = P_NO_ERR;
  if (P_ERR==Puint32_acc_add (pads,&(acc->nerr),&tpd,&(pd->nerr))) 
    {
      nerr++;
    }
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Pstring_acc_add (pads,&(acc->state),&(pd->state),&(rep->state))) 
        {
          nerr++;
        }
      if (P_ERR==Puint32_acc_add (pads,&(acc->tstamp),&(pd->tstamp),&(rep->tstamp))) 
        {
          nerr++;
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t event_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,event_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "struct event_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_STRUCT_ACC_REP_NOVALS ();
  if (P_ERR==P_nerr_acc_report2io (pads,outstr,"Errors","errors",-1,&(acc->nerr))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.state",prefix);
  if (P_ERR==Pstring_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->state))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.tstamp",prefix);
  if (P_ERR==Puint32_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->tstamp))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t event_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,event_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = event_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t event_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,event_t_pd *pd,event_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("event_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  tlen_PCGEN_ = Pstring_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->state),&(rep->state),124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint32_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->tstamp),&(rep->tstamp));
  PCGEN_FINAL_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t event_t_write2io (P_t *pads,Sfio_t *io,event_t_pd *pd,event_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("event_t_write2io",event_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t event_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,event_t_pd *pd,event_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("event_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("event_t");
  PCGEN_STRUCT_PD_XML_OUT ();
  tlen_PCGEN_ = Pstring_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->state),&(rep->state),"state",indent+2,124);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = Puint32_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->tstamp),&(rep->tstamp),"tstamp",indent+2);
  PCGEN_TLEN_UPDATES ();
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t event_t_write_xml_2io (P_t *pads,Sfio_t *io,event_t_pd *pd,event_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("event_t_write_xml_2io",event_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t event_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,event_t_m *m,event_t_pd *pd,event_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("event_t_fmt2buf_final");
  PCGEN_FMT2BUF_STRUCT_FIELD ("Pstring_fmt2buf",Pstring_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->state),&(pd->state),&(rep->state),124));
  PCGEN_FMT2BUF_STRUCT_FIELD ("Puint32_fmt2buf",Puint32_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->tstamp),&(pd->tstamp),&(rep->tstamp)));
  PCGEN_FMT2BUF_FIX_LAST ();
  return length_PCGEN_;
}
ssize_t event_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,event_t_m *m,event_t_pd *pd,event_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("event_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"event_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return event_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t event_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,event_t_m *m,event_t_pd *pd,event_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("event_t_fmt2io",event_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
Perror_t eventSeq_t_init (P_t *pads,eventSeq_t *rep)
{
  PDCI_DISC_1P_CHECKS ("eventSeq_t_init",rep);
  memset ((void *) rep,0,sizeof(eventSeq_t));
  return P_OK;
}
Perror_t eventSeq_t_pd_init (P_t *pads,eventSeq_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("eventSeq_t_pd_init",pd);
  memset ((void *) pd,0,sizeof(eventSeq_t_pd));
  return P_OK;
}
Perror_t eventSeq_t_cleanup (P_t *pads,eventSeq_t *rep)
{
  PDCI_DISC_1P_CHECKS ("eventSeq_t_cleanup",rep);
  {
    Puint32 nerr_PCGEN_=0;
    PCGEN_ARRAY_CLEANUP_AR_DYN_ELT_DYN ("eventSeq_t_cleanup",rep,event_t_cleanup);
    return (nerr_PCGEN_==0) ? P_OK : P_ERR;
  }
}
Perror_t eventSeq_t_pd_cleanup (P_t *pads,eventSeq_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("eventSeq_t_pd_cleanup",pd);
  {
    Puint32 nerr_PCGEN_=0;
    PCGEN_ARRAY_CLEANUP_AR_DYN_ELT_DYN ("eventSeq_t_pd_cleanup",pd,event_t_pd_cleanup);
    return (nerr_PCGEN_==0) ? P_OK : P_ERR;
  }
}
Perror_t eventSeq_t_copy (P_t *pads,eventSeq_t *rep_dst,eventSeq_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("eventSeq_t_copy",rep_src,rep_dst);
  {
    Puint32 nerr_PCGEN_=0;
    PCGEN_ARRAY_COPY_AR_DYN_ELT_DYN ("eventSeq_t_copy",rep_src,rep_dst,event_t_copy,event_t_cleanup);
    return (nerr_PCGEN_==0) ? P_OK : P_ERR;
  }
}
Perror_t eventSeq_t_pd_copy (P_t *pads,eventSeq_t_pd *pd_dst,eventSeq_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("eventSeq_t_pd_copy",pd_src,pd_dst);
  {
    Puint32 nerr_PCGEN_=0;
    PCGEN_ARRAY_COPY_AR_DYN_ELT_DYN ("eventSeq_t_pd_copy",pd_src,pd_dst,event_t_pd_copy,event_t_pd_cleanup);
    return (nerr_PCGEN_==0) ? P_OK : P_ERR;
  }
}
void eventSeq_t_m_init (P_t *pads,eventSeq_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(eventSeq_t_m));
}
Perror_t eventSeq_t_read_old (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("eventSeq_t_read_old",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    Pregexp_t separator_regexp={0,{0,0,0,0,0},{{0,0}}};
    Pregexp_t *separator_regexp_ptr=&separator_regexp;
    Pregexp_t terminator_regexp={0,{0,0,0,0,0},{{0,0}}};
    Pregexp_t *terminator_regexp_ptr=&terminator_regexp;
    Ploc_t tloc;
    Ploc_t *loc_ptr=&tloc;
    int result;
    int foundTerm=0;
    rep->length = 0;
    pd->neerr = 0;
    pd->firstError = 0;
    pd->numRead = 0;
    if (P_ERR==PDCI_regexp_compile_cstr (pads,P_RE_STRING_FROM_CHAR (pads,124),separator_regexp_ptr,"Array separator","eventSeq_t_read")) 
      {
        pd->errCode = P_INVALID_REGEXP;
        (pd->nerr)+=1;
        P_PS_setPanic (pd);
        goto eventSeq_t_end;
      }
    if (P_ERR==PDCI_regexp_compile_cstr (pads,"/$/",terminator_regexp_ptr,"Array terminator","eventSeq_t_read")) 
      {
        pd->errCode = P_INVALID_REGEXP;
        (pd->nerr)+=1;
        P_PS_setPanic (pd);
        goto eventSeq_t_end;
      }
    P_io_getLocB (pads,loc_ptr,0);
    if (0==(rep->_internal)) 
      {
        rep->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(rep->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"eventSeq_t_read","");
          }
      }
    if (0==(pd->_internal)) 
      {
        pd->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(pd->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"eventSeq_t_read","");
          }
      }
    // Read input until we reach a termination condition
    if ((!P_PS_isPanic (pd))&&(!P_io_at_eof (pads))) 
      {
        if (P_OK==Pre_match (pads,terminator_regexp_ptr,0)) 
          {
            foundTerm = 1;
          }
        else
          {
            {
              P_io_getLocB (pads,&(pd->loc),0);
              while (1)
                {
                  // Ready to read next element
                  (rep->length)++;
                  if (0!=RBuf_reserve (rep->_internal,(void **) (&(rep->elts)),sizeof(event_t),rep->length,0)) 
                    {
                      PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"eventSeq_t_read",0);
                    }
                  if (0!=RBuf_reserve (pd->_internal,(void **) (&(pd->elts)),sizeof(event_t_pd),rep->length,0)) 
                    {
                      PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"eventSeq_t_read",0);
                    }
                  if ((rep->length)>1) 
                    {
                      P_io_getLocB (pads,&((pd->elts)[rep->length].loc),0);
                      if (P_POS_EQ (((pd->elts)[(rep->length)-1].loc).b,((pd->elts)[rep->length].loc).b)) 
                        {
                          // array termination from lack of progress
                          (rep->length)-=2;
                          break;
                        }
                    }
                  result = event_t_read (pads,&(m->element),&(pd->elts)[(rep->length)-1],&(rep->elts)[(rep->length)-1]);
                  (pd->numRead)++;
                  if (result==P_ERR) 
                    {
                      // in markErrorSs
                      if (P_Test_NotIgnore (m->compoundLevel)) 
                        {
                          (pd->neerr)++;
                          if (!(pd->nerr)) 
                            {
                              (pd->nerr)++;
                              pd->errCode = P_ARRAY_ELEM_ERR;
                              P_io_getLocE (pads,&(pd->loc),-1);
                              // Index of first element with an error
                              pd->firstError = ((rep->length)-1);
                            }
                          if (P_spec_level (pads)) 
                            return P_ERR;
                        }
                    }
                  if (P_PS_isPanic (&(pd->elts)[(rep->length)-1])) 
                    {
                      {
                        int f_found;
                        size_t offset;
                        // Try to recover to separator and/or terminator
                        if (P_OK==Pre_scan2 (pads,separator_regexp_ptr,terminator_regexp_ptr,1,0,1,&f_found,&offset)) 
                          {
                            // We recovered; restored invariant
                          }
                        else
                          {
                            // Recovery failed
                            P_PS_setPanic (pd);
                            break;
                          }
                      }
                    }
                  // Looking for terminator
                  {
                    if (P_OK==Pre_match (pads,terminator_regexp_ptr,0)) 
                      {
                        foundTerm = 1;
                      }
                  }
                  // Have we finished reading array?
                  if (P_io_at_eof (pads)||foundTerm) 
                    {
                      break;
                    }
                  // Array not finished; read separator with recovery to terminator
                  {
                    int f_found;
                    size_t offset;
                    P_io_getLocB (pads,&(pd->loc),0);
                    if (P_OK==Pre_scan2 (pads,separator_regexp_ptr,terminator_regexp_ptr,1,0,0,&f_found,&offset)) 
                      {
                        if (!f_found) 
                          {
                            foundTerm = 1;
                          }
                        if (P_Test_SynCheck (m->compoundLevel)) 
                          {
                            if (f_found&&offset) 
                              {
                                if (!(pd->nerr)) 
                                  {
                                    (pd->nerr)++;
                                    pd->errCode = P_ARRAY_EXTRA_BEFORE_SEP;
                                    P_io_getLocE (pads,&(pd->loc),-2);
                                    PDCI_report_err (pads,P_LEV_WARN,&(pd->loc),pd->errCode,"eventSeq_t_read",0);
                                  }
                                else
                                  {
                                    (pd->nerr)++;
                                  }
                                if (P_spec_level (pads)) 
                                  return P_ERR;
                              }
                            else
                              {
                                if (!f_found) 
                                  {
                                    {
                                      if (!(pd->nerr)) 
                                        {
                                          (pd->nerr)++;
                                          pd->errCode = P_ARRAY_EXTRA_BEFORE_TERM;
                                          P_io_getLocE (pads,&(pd->loc),-1);
                                          PDCI_report_err (pads,P_LEV_WARN,&(pd->loc),pd->errCode,"eventSeq_t_read",0);
                                        }
                                      else
                                        {
                                          (pd->nerr)++;
                                        }
                                      if (P_spec_level (pads)) 
                                        return P_ERR;
                                    }
                                    break;
                                  }
                              }
                          }
                      }
                    else
                      {
                        // Error reading separator
                        {
                          if (!(pd->nerr)) 
                            {
                              (pd->nerr)++;
                              pd->errCode = P_ARRAY_SEP_ERR;
                              P_io_getLocE (pads,&(pd->loc),-1);
                              PDCI_report_err (pads,P_LEV_WARN,&(pd->loc),pd->errCode,"eventSeq_t_read","Missing separator");
                            }
                          else
                            {
                              (pd->nerr)++;
                            }
                          if (P_spec_level (pads)) 
                            return P_ERR;
                          P_PS_setPanic (pd);
                        }
                        break;
                      }
                  }
                }
            }
          }
      }
    // End of loop. Read trailing terminator if there was trailing junk
    if ((!P_PS_isPanic (pd))&&(!foundTerm)) 
      {
        size_t offset;
        P_io_getLocB (pads,&(pd->loc),0);
        if (P_OK==Pre_scan1 (pads,terminator_regexp_ptr,0,0,&offset)) 
          {
            if (P_Test_SynCheck (m->compoundLevel)) 
              {
                {
                  if (!(pd->nerr)) 
                    {
                      (pd->nerr)++;
                      pd->errCode = P_ARRAY_EXTRA_BEFORE_TERM;
                      P_io_getLocE (pads,&(pd->loc),-1);
                      PDCI_report_err (pads,P_LEV_WARN,&(pd->loc),pd->errCode,"eventSeq_t_read",0);
                    }
                  else
                    {
                      (pd->nerr)++;
                    }
                  if (P_spec_level (pads)) 
                    return P_ERR;
                }
                foundTerm = 1;
              }
          }
        else
          {
            if (!(pd->nerr)) 
              {
                (pd->nerr)++;
                pd->errCode = P_ARRAY_TERM_ERR;
                P_io_getLocE (pads,&(pd->loc),-1);
                PDCI_report_err (pads,P_LEV_WARN,&(pd->loc),pd->errCode,"eventSeq_t_read","Missing terminator");
              }
            else
              {
                (pd->nerr)++;
              }
            if (P_spec_level (pads)) 
              return P_ERR;
            P_PS_setPanic (pd);
          }
      }
    pd->length = (rep->length);
    // Checking user-defined array constraints
    if (P_Test_SemCheck (m->compoundLevel)&&(!P_PS_isPanic (pd))) 
      {
        // Checking Pforall constraint
        {
          int violated=0;
          {
            int i;
            if (!((0<=0)&&(((rep->length)-2)<(rep->length)))) 
              {
                violated = 1;
              }
            for (i = 0; (!violated)&&(i<=((rep->length)-2)); i++)
              {
                if (!(((rep->elts)[i].tstamp)<=((rep->elts)[i+1].tstamp))) 
                  {
                    violated = 1;
                  }
              }
          }
          if (violated) 
            {
              if (!(pd->nerr)) 
                {
                  (pd->nerr)++;
                  pd->errCode = P_ARRAY_USER_CONSTRAINT_ERR;
                  P_io_getLocE (pads,loc_ptr,-1);
                  PDCI_report_err (pads,P_LEV_WARN,loc_ptr,pd->errCode,"eventSeq_t_read","Pforall constraint for array eventSeq_t violated");
                }
              else
                {
                  (pd->nerr)++;
                }
              if (P_spec_level (pads)) 
                return P_ERR;
            }
        }
      }
    
  eventSeq_t_end: 
    {
      Pregexp_cleanup (pads,separator_regexp_ptr);
      Pregexp_cleanup (pads,terminator_regexp_ptr);
    }
    return ((pd->nerr)==0) ? P_OK : P_ERR;
  }
}
void eventSeq_t_ro_params_init (eventSeq_t_ro_params_t *params)
{
  Pregexp_t separator_regexp={0,{0,0,0,0,0},{{0,0}}};
  Pregexp_t terminator_regexp={0,{0,0,0,0,0},{{0,0}}};
  params->separator_regexp = separator_regexp;
  params->separator_regexp_ptr = (&(params->separator_regexp));
  params->terminator_regexp = terminator_regexp;
  params->terminator_regexp_ptr = (&(params->terminator_regexp));
}
Pread_res_t eventSeq_t_final_checks (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep,Ploc_t *loc_ptr,Pregexp_t *separator_regexp_ptr,Pregexp_t *terminator_regexp_ptr,int foundTerm,int consume)
{
  PDCI_IODISC_3P_CHECKS ("eventSeq_t_final_checks",m,pd,rep);
  {
    PCGEN_ARRAY_UNSET_PARTIAL ();
    PCGEN_ARRAY_TEST_TRAILING_JUNK_P (eventSeq_t,Pre_scan1,terminator_regexp_ptr);
    pd->length = (rep->length);
    // Checking user-defined array constraints
    if (P_Test_SemCheck (m->compoundLevel)&&(!P_PS_isPanic (pd))) 
      {
        // Checking Pforall constraint
        {
          int violated=0;
          {
            int i;
            if (!((0<=0)&&(((rep->length)-2)<(rep->length)))) 
              {
                violated = 1;
              }
            for (i = 0; (!violated)&&(i<=((rep->length)-2)); i++)
              {
                if (!(((rep->elts)[i].tstamp)<=((rep->elts)[i+1].tstamp))) 
                  {
                    violated = 1;
                  }
              }
          }
          if (violated) 
            {
              if (!(pd->nerr)) 
                {
                  (pd->nerr)++;
                  pd->errCode = P_ARRAY_USER_CONSTRAINT_ERR;
                  P_io_getLocE (pads,loc_ptr,-1);
                  PDCI_report_err (pads,P_LEV_WARN,loc_ptr,pd->errCode,"eventSeq_t_read","Pforall constraint for array eventSeq_t violated");
                }
              else
                {
                  (pd->nerr)++;
                }
              if (P_spec_level (pads)) 
                return P_READ_ERR;
            }
        }
      }
    Pregexp_cleanup (pads,separator_regexp_ptr);
    Pregexp_cleanup (pads,terminator_regexp_ptr);
    return PCGEN_ARRAY_RET_DONE (consume);
  }
}
Pread_res_t eventSeq_t_read_one_init (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep,Ploc_t *loc_ptr,Pregexp_t *separator_regexp_ptr,Pregexp_t *terminator_regexp_ptr)
{
  PDCI_IODISC_3P_CHECKS ("eventSeq_t_read_one_init",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    int foundTerm=0;
    rep->length = 0;
    pd->neerr = 0;
    pd->firstError = 0;
    pd->numRead = 0;
    if (P_ERR==PDCI_regexp_compile_cstr (pads,P_RE_STRING_FROM_CHAR (pads,124),separator_regexp_ptr,"Array separator","eventSeq_t_read")) 
      {
        pd->errCode = P_INVALID_REGEXP;
        (pd->nerr)+=1;
        P_PS_setPanic (pd);
        goto eventSeq_t_end;
      }
    if (P_ERR==PDCI_regexp_compile_cstr (pads,"/$/",terminator_regexp_ptr,"Array terminator","eventSeq_t_read")) 
      {
        pd->errCode = P_INVALID_REGEXP;
        (pd->nerr)+=1;
        P_PS_setPanic (pd);
        goto eventSeq_t_end;
      }
    P_io_getLocB (pads,loc_ptr,0);
    if (0==(rep->_internal)) 
      {
        rep->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(rep->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"eventSeq_t_read","");
          }
      }
    if (0==(pd->_internal)) 
      {
        pd->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(pd->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"eventSeq_t_read","");
          }
      }
    if (P_PS_isPanic (pd)||P_io_at_eof (pads)) 
      PCGEN_ARRAY_DO_FINAL_CHECKS ();
    if (P_OK==Pre_match (pads,terminator_regexp_ptr,0)) 
      {
        foundTerm = 1;
        PCGEN_ARRAY_DO_FINAL_CHECKS ();
      }
    P_io_getLocB (pads,&(pd->loc),0);
    PCGEN_ARRAY_SET_PARTIAL ();
    return PCGEN_ARRAY_RET_ONGOING (0);
    PCGEN_ARRAY_LBL_FINAL_CHECKS ();
    return eventSeq_t_final_checks (pads,m,pd,rep,loc_ptr,separator_regexp_ptr,terminator_regexp_ptr,foundTerm,0);
    
  eventSeq_t_end: 
    {
      Pregexp_cleanup (pads,separator_regexp_ptr);
      Pregexp_cleanup (pads,terminator_regexp_ptr);
    }
    PCGEN_ARRAY_UNSET_PARTIAL ();
    return P_READ_ERR;
  }
}
Pread_res_t eventSeq_t_read_one (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep,Ploc_t *loc_ptr,event_t_pd *elt_pd,event_t *elt_rep,Pregexp_t *separator_regexp_ptr,Pregexp_t *terminator_regexp_ptr)
{
  PDCI_IODISC_3P_CHECKS ("eventSeq_t_read_one",m,pd,rep);
  PDCI_IODISC_2P_CHECKS ("eventSeq_t_read_one",elt_pd,elt_rep);
  {
    int foundTerm=0;
    int haveData=0;
    PCGEN_ARRAY_RO_DECS ();
    PCGEN_ARRAY_TEST_ALREADY_DONE ();
    PCGEN_ARRAY_GET_BEGIN_LOC ();
    // Ready to read next element
    if ((pd->numRead)>0) 
      {
        // Array not finished; read separator with recovery to terminator
        {
          int f_found;
          size_t offset;
          P_io_getLocB (pads,&(pd->loc),0);
          if (P_OK==Pre_scan2 (pads,separator_regexp_ptr,terminator_regexp_ptr,1,0,0,&f_found,&offset)) 
            {
              if (!f_found) 
                {
                  foundTerm = 1;
                }
              if (P_Test_SynCheck (m->compoundLevel)) 
                {
                  if (f_found&&offset) 
                    {
                      if (!(pd->nerr)) 
                        {
                          (pd->nerr)++;
                          pd->errCode = P_ARRAY_EXTRA_BEFORE_SEP;
                          P_io_getLocE (pads,&(pd->loc),-2);
                          PDCI_report_err (pads,P_LEV_WARN,&(pd->loc),pd->errCode,"eventSeq_t_read",0);
                        }
                      else
                        {
                          (pd->nerr)++;
                        }
                      if (P_spec_level (pads)) 
                        return P_READ_ERR;
                    }
                  else
                    {
                      if (!f_found) 
                        {
                          {
                            if (!(pd->nerr)) 
                              {
                                (pd->nerr)++;
                                pd->errCode = P_ARRAY_EXTRA_BEFORE_TERM;
                                P_io_getLocE (pads,&(pd->loc),-1);
                                PDCI_report_err (pads,P_LEV_WARN,&(pd->loc),pd->errCode,"eventSeq_t_read",0);
                              }
                            else
                              {
                                (pd->nerr)++;
                              }
                            if (P_spec_level (pads)) 
                              return P_READ_ERR;
                          }
                          PCGEN_ARRAY_DO_FINAL_CHECKS ();
                        }
                    }
                }
            }
          else
            {
              // Error reading separator
              {
                if (!(pd->nerr)) 
                  {
                    (pd->nerr)++;
                    pd->errCode = P_ARRAY_SEP_ERR;
                    P_io_getLocE (pads,&(pd->loc),-1);
                    PDCI_report_err (pads,P_LEV_WARN,&(pd->loc),pd->errCode,"eventSeq_t_read","Missing separator");
                  }
                else
                  {
                    (pd->nerr)++;
                  }
                if (P_spec_level (pads)) 
                  return P_READ_ERR;
                P_PS_setPanic (pd);
              }
              PCGEN_ARRAY_DO_FINAL_CHECKS ();
            }
        }
      }
    PCGEN_ARRAY_READ_ELEM_HD (event_t_read (pads,&(m->element),elt_pd,elt_rep),haveData);
    PCGEN_ARRAY_TEST_READ_ERR (1,1);
    if (P_PS_isPanic (elt_pd)) 
      {
        {
          int f_found;
          size_t offset;
          // Try to recover to separator and/or terminator
          if (P_OK==Pre_scan2 (pads,separator_regexp_ptr,terminator_regexp_ptr,1,0,1,&f_found,&offset)) 
            {
              // We recovered; restored invariant
            }
          else
            {
              // Recovery failed
              P_PS_setPanic (pd);
              PCGEN_ARRAY_DO_FINAL_CHECKS ();
            }
        }
      }
    // Looking for terminator
    {
      if (P_OK==Pre_match (pads,terminator_regexp_ptr,0)) 
        {
          foundTerm = 1;
        }
    }
    // Have we finished reading array?
    if (P_io_at_eof (pads)||foundTerm) 
      {
        PCGEN_ARRAY_DO_FINAL_CHECKS ();
      }
    PCGEN_ARRAY_TEST_FC_SOURCE_ADVANCE2 ();
    return PCGEN_ARRAY_RET_ONGOING (1);
    PCGEN_ARRAY_LBL_FINAL_CHECKS ();
    return eventSeq_t_final_checks (pads,m,pd,rep,loc_ptr,separator_regexp_ptr,terminator_regexp_ptr,foundTerm,haveData);
  }
}
Perror_t eventSeq_t_read (P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("eventSeq_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    Pregexp_t separator_regexp={0,{0,0,0,0,0},{{0,0}}};
    Pregexp_t *separator_regexp_ptr=&separator_regexp;
    Pregexp_t terminator_regexp={0,{0,0,0,0,0},{{0,0}}};
    Pregexp_t *terminator_regexp_ptr=&terminator_regexp;
    Ploc_t tloc;
    Ploc_t *loc_ptr=&tloc;
    int i=0;
    int result;
    eventSeq_t_read_one_init (pads,m,pd,rep,loc_ptr,separator_regexp_ptr,terminator_regexp_ptr);
    PCGEN_ARRAY_READ_ALL (PCGEN_ARRAY_RESERVE_SPACE (eventSeq_t,event_t,event_t_pd,0),eventSeq_t_read_one (pads,m,pd,rep,loc_ptr,&(pd->elts)[i],&(rep->elts)[i],separator_regexp_ptr,terminator_regexp_ptr),i = (rep->length),"eventSeq_t_read");
    return PCGEN_ARRAY_STD_RETURN ();
  }
}
Pread_res_t eventSeq_t_reread_one (Pregexp_t *separator_regexp_ptr,Pregexp_t *terminator_regexp_ptr,P_t *pads,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep,Ploc_t *loc_ptr,event_t_pd *elt_pd,event_t *elt_rep,int notFirstElt)
{
  PDCI_IODISC_3P_CHECKS ("eventSeq_t_reread_one",m,pd,rep);
  PDCI_IODISC_2P_CHECKS ("eventSeq_t_reread_one",elt_pd,elt_rep);
  {
    // Ready to read element
    if (notFirstElt) 
      {
        // Array not finished; read separator with recovery to terminator
        {
          int f_found;
          size_t offset;
          if (P_ERR==Pre_scan2 (pads,separator_regexp_ptr,terminator_regexp_ptr,1,0,0,&f_found,&offset)) 
            return P_READ_ERR;
        }
      }
    PCGEN_ARRAY_REREAD_ELEM_BODY (event_t_read (pads,&(m->element),elt_pd,elt_rep));
    return PCGEN_ARRAY_REREAD_ELEM_RET ();
  }
}
int is_eventSeq_t (eventSeq_t *rep)
{
  int violated=0;
  {
    int i;
    for (i = 0; (!violated)&&(i<(rep->length)); i++)
      {
        if (!is_event_t (&(rep->elts)[i])) 
          {
            violated = 1;
          }
      }
  }
  {
    int i;
    if (!((0<=0)&&(((rep->length)-2)<(rep->length)))) 
      {
        violated = 1;
      }
    for (i = 0; (!violated)&&(i<=((rep->length)-2)); i++)
      {
        if (!(((rep->elts)[i].tstamp)<=((rep->elts)[i+1].tstamp))) 
          {
            violated = 1;
          }
      }
  }
  return !violated;
}
Perror_t eventSeq_t_acc_init (P_t *pads,eventSeq_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->length))) 
    {
      nerr++;
    }
  if (P_ERR==event_t_acc_init (pads,&(acc->compoundLevel))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        if (P_ERR==event_t_acc_init (pads,&(acc->arrayDetail)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t eventSeq_t_acc_reset (P_t *pads,eventSeq_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->length))) 
    {
      nerr++;
    }
  if (P_ERR==event_t_acc_reset (pads,&(acc->compoundLevel))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        if (P_ERR==event_t_acc_reset (pads,&(acc->arrayDetail)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t eventSeq_t_acc_cleanup (P_t *pads,eventSeq_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->length))) 
    {
      nerr++;
    }
  if (P_ERR==event_t_acc_cleanup (pads,&(acc->compoundLevel))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        if (P_ERR==event_t_acc_cleanup (pads,&(acc->arrayDetail)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t eventSeq_t_acc_add (P_t *pads,eventSeq_t_acc *acc,eventSeq_t_pd *pd,eventSeq_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = (pd->errCode);
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Puint32_acc_add (pads,&(acc->length),&tpd,&(rep->length))) 
        {
          nerr++;
        }
      {
        int i;
        for (i = 0; i<(rep->length); i++)
          {
            if (i<10) 
              {
                if (P_ERR==event_t_acc_add (pads,&(acc->arrayDetail)[i],&(pd->elts)[i],&(rep->elts)[i])) 
                  {
                    nerr++;
                  }
              }
            if (P_ERR==event_t_acc_add (pads,&(acc->compoundLevel),&(pd->elts)[i],&(rep->elts)[i])) 
              {
                nerr++;
              }
          }
      }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t eventSeq_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,eventSeq_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "array eventSeq_t of event_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_ARRAY_ACC_REP_NOVALS ();
  if (P_ERR==Puint32_acc_report2io (pads,outstr,"Array lengths","array length",-1,&(acc->length))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.allArrayElts",prefix);
  if (P_ERR==event_t_acc_report2io (pads,outstr,sfstruse (tmpstr),"all array element",nst,&(acc->compoundLevel))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  {
    int i;
    for (i = 0; i<((((acc->length).max)<10) ? (acc->length).max : 10); i++)
      {
        sfprintf (tmpstr,"%s.arrayDetail[%d]",prefix,i);
        if (P_ERR==event_t_acc_report2io (pads,outstr,sfstruse (tmpstr),"array element",nst,&(acc->arrayDetail)[i])) 
          {
            sfstrclose (tmpstr);
            return P_ERR;
          }
      }
  }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t eventSeq_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,eventSeq_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = eventSeq_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t eventSeq_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,eventSeq_t_pd *pd,eventSeq_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("eventSeq_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  {
    int i=0;
    if ((rep->length)>1) 
      {
        for (i = 0; i<((rep->length)-1); i++)
          {
            tlen_PCGEN_ = event_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->elts)[i],&(rep->elts)[i]);
            PCGEN_TLEN_UPDATES ();
            tlen_PCGEN_ = Pchar_lit_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,124);
            PCGEN_TLEN_UPDATES ();
          }
      }
    if ((rep->length)!=0) 
      {
        tlen_PCGEN_ = event_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->elts)[i],&(rep->elts)[i]);
        PCGEN_TLEN_UPDATES ();
      }
  }
  // Don't currently support writing regular expressions
  return length_PCGEN_;
}
ssize_t eventSeq_t_write2io (P_t *pads,Sfio_t *io,eventSeq_t_pd *pd,eventSeq_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("eventSeq_t_write2io",eventSeq_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t eventSeq_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,eventSeq_t_pd *pd,eventSeq_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("eventSeq_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("eventSeq_t");
  PCGEN_ARRAY_PD_XML_OUT ();
  {
    int i=0;
    for (i = 0; i<(rep->length); i++)
      {
        tlen_PCGEN_ = event_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->elts)[i],&(rep->elts)[i],"elt",indent+2);
        PCGEN_TLEN_UPDATES ();
      }
  }
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t eventSeq_t_write_xml_2io (P_t *pads,Sfio_t *io,eventSeq_t_pd *pd,eventSeq_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("eventSeq_t_write_xml_2io",eventSeq_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t eventSeq_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  int i=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("eventSeq_t_fmt2buf_final");
  PCGEN_FMT2BUF_ARRAY ("eventSeq_t_fmt2buf_final",event_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->element),&(pd->elts)[i],&(rep->elts)[i]));
  PCGEN_FMT2BUF_FIX_LAST ();
  return length_PCGEN_;
}
ssize_t eventSeq_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("eventSeq_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"eventSeq_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return eventSeq_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t eventSeq_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,eventSeq_t_m *m,eventSeq_t_pd *pd,eventSeq_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("eventSeq_t_fmt2io",eventSeq_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
Perror_t entry_t_init (P_t *pads,entry_t *rep)
{
  PDCI_DISC_1P_CHECKS ("entry_t_init",rep);
  memset ((void *) rep,0,sizeof(entry_t));
  return P_OK;
}
Perror_t entry_t_pd_init (P_t *pads,entry_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("entry_t_pd_init",pd);
  memset ((void *) pd,0,sizeof(entry_t_pd));
  return P_OK;
}
Perror_t entry_t_cleanup (P_t *pads,entry_t *rep)
{
  PDCI_DISC_1P_CHECKS ("entry_t_cleanup",rep);
  order_header_t_cleanup (pads,&(rep->h));
  eventSeq_t_cleanup (pads,&(rep->events));
  return P_OK;
}
Perror_t entry_t_pd_cleanup (P_t *pads,entry_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("entry_t_pd_cleanup",pd);
  order_header_t_pd_cleanup (pads,&(pd->h));
  eventSeq_t_pd_cleanup (pads,&(pd->events));
  return P_OK;
}
Perror_t entry_t_copy (P_t *pads,entry_t *rep_dst,entry_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("entry_t_copy",rep_src,rep_dst);
  order_header_t_copy (pads,&(rep_dst->h),&(rep_src->h));
  eventSeq_t_copy (pads,&(rep_dst->events),&(rep_src->events));
  return P_OK;
}
Perror_t entry_t_pd_copy (P_t *pads,entry_t_pd *pd_dst,entry_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("entry_t_pd_copy",pd_src,pd_dst);
  order_header_t_pd_copy (pads,&(pd_dst->h),&(pd_src->h));
  eventSeq_t_pd_copy (pads,&(pd_dst->events),&(pd_src->events));
  return P_OK;
}
void entry_t_m_init (P_t *pads,entry_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(entry_t_m));
}
Perror_t entry_t_read (P_t *pads,entry_t_m *m,entry_t_pd *pd,entry_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("entry_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  // Read field 'h'
  PCGEN_STRUCT_READ_FIRST ("entry_t_read",h,order_header_t_read (pads,&(m->h),&(pd->h),&(rep->h)),_NOOP);
  // Read field 'events'
  PCGEN_STRUCT_READ_NEXT ("entry_t_read",events,eventSeq_t_read (pads,&(m->events),&(pd->events),&(rep->events)),_NOOP);
  PCGEN_FIND_EOR ("entry_t_read");
  return ((pd->nerr)==0) ? P_OK : P_ERR;
}
int is_entry_t (entry_t *rep)
{
  return is_order_header_t (&(rep->h))&&(is_eventSeq_t (&(rep->events))&&1);
}
Perror_t entry_t_acc_init (P_t *pads,entry_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==order_header_t_acc_init (pads,&(acc->h))) 
    {
      nerr++;
    }
  if (P_ERR==eventSeq_t_acc_init (pads,&(acc->events))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t entry_t_acc_reset (P_t *pads,entry_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==order_header_t_acc_reset (pads,&(acc->h))) 
    {
      nerr++;
    }
  if (P_ERR==eventSeq_t_acc_reset (pads,&(acc->events))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t entry_t_acc_cleanup (P_t *pads,entry_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==order_header_t_acc_cleanup (pads,&(acc->h))) 
    {
      nerr++;
    }
  if (P_ERR==eventSeq_t_acc_cleanup (pads,&(acc->events))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t entry_t_acc_add (P_t *pads,entry_t_acc *acc,entry_t_pd *pd,entry_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = P_NO_ERR;
  if (P_ERR==Puint32_acc_add (pads,&(acc->nerr),&tpd,&(pd->nerr))) 
    {
      nerr++;
    }
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==order_header_t_acc_add (pads,&(acc->h),&(pd->h),&(rep->h))) 
        {
          nerr++;
        }
      if (P_ERR==eventSeq_t_acc_add (pads,&(acc->events),&(pd->events),&(rep->events))) 
        {
          nerr++;
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t entry_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,entry_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "struct entry_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_STRUCT_ACC_REP_NOVALS ();
  if (P_ERR==P_nerr_acc_report2io (pads,outstr,"Errors","errors",-1,&(acc->nerr))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.h",prefix);
  if (P_ERR==order_header_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->h))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.events",prefix);
  if (P_ERR==eventSeq_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->events))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t entry_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,entry_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = entry_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t entry_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entry_t_pd *pd,entry_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("entry_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  tlen_PCGEN_ = PDCI_io_rec_open_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,"entry_t_write2buf");
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = order_header_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->h),&(rep->h));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = eventSeq_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->events),&(rep->events));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = PDCI_io_rec_close_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,buf,length_PCGEN_,"entry_t_write2buf");
  PCGEN_FINAL_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t entry_t_write2io (P_t *pads,Sfio_t *io,entry_t_pd *pd,entry_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("entry_t_write2io",entry_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t entry_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entry_t_pd *pd,entry_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("entry_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("entry_t");
  PCGEN_STRUCT_PD_XML_OUT ();
  tlen_PCGEN_ = order_header_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->h),&(rep->h),"h",indent+2);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = eventSeq_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->events),&(rep->events),"events",indent+2);
  PCGEN_TLEN_UPDATES ();
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t entry_t_write_xml_2io (P_t *pads,Sfio_t *io,entry_t_pd *pd,entry_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("entry_t_write_xml_2io",entry_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t entry_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,entry_t_m *m,entry_t_pd *pd,entry_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("entry_t_fmt2buf_final");
  PCGEN_FMT2BUF_STRUCT_FIELD ("order_header_t_fmt2buf",order_header_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->h),&(pd->h),&(rep->h)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("eventSeq_t_fmt2buf",eventSeq_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->events),&(pd->events),&(rep->events)));
  PCGEN_FMT2BUF_FIX_LAST ();
  PCGEN_FMT2BUF_RECORD ("entry_t_fmt2buf_final");
  return length_PCGEN_;
}
ssize_t entry_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,entry_t_m *m,entry_t_pd *pd,entry_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("entry_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"entry_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return entry_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t entry_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,entry_t_m *m,entry_t_pd *pd,entry_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("entry_t_fmt2io",entry_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
Perror_t entries_t_init (P_t *pads,entries_t *rep)
{
  PDCI_DISC_1P_CHECKS ("entries_t_init",rep);
  memset ((void *) rep,0,sizeof(entries_t));
  return P_OK;
}
Perror_t entries_t_pd_init (P_t *pads,entries_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("entries_t_pd_init",pd);
  memset ((void *) pd,0,sizeof(entries_t_pd));
  return P_OK;
}
Perror_t entries_t_cleanup (P_t *pads,entries_t *rep)
{
  PDCI_DISC_1P_CHECKS ("entries_t_cleanup",rep);
  {
    Puint32 nerr_PCGEN_=0;
    PCGEN_ARRAY_CLEANUP_AR_DYN_ELT_DYN ("entries_t_cleanup",rep,entry_t_cleanup);
    return (nerr_PCGEN_==0) ? P_OK : P_ERR;
  }
}
Perror_t entries_t_pd_cleanup (P_t *pads,entries_t_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("entries_t_pd_cleanup",pd);
  {
    Puint32 nerr_PCGEN_=0;
    PCGEN_ARRAY_CLEANUP_AR_DYN_ELT_DYN ("entries_t_pd_cleanup",pd,entry_t_pd_cleanup);
    return (nerr_PCGEN_==0) ? P_OK : P_ERR;
  }
}
Perror_t entries_t_copy (P_t *pads,entries_t *rep_dst,entries_t *rep_src)
{
  PDCI_DISC_2P_CHECKS ("entries_t_copy",rep_src,rep_dst);
  {
    Puint32 nerr_PCGEN_=0;
    PCGEN_ARRAY_COPY_AR_DYN_ELT_DYN ("entries_t_copy",rep_src,rep_dst,entry_t_copy,entry_t_cleanup);
    return (nerr_PCGEN_==0) ? P_OK : P_ERR;
  }
}
Perror_t entries_t_pd_copy (P_t *pads,entries_t_pd *pd_dst,entries_t_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("entries_t_pd_copy",pd_src,pd_dst);
  {
    Puint32 nerr_PCGEN_=0;
    PCGEN_ARRAY_COPY_AR_DYN_ELT_DYN ("entries_t_pd_copy",pd_src,pd_dst,entry_t_pd_copy,entry_t_pd_cleanup);
    return (nerr_PCGEN_==0) ? P_OK : P_ERR;
  }
}
void entries_t_m_init (P_t *pads,entries_t_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(entries_t_m));
}
Perror_t entries_t_read_old (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("entries_t_read_old",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    Ploc_t tloc;
    Ploc_t *loc_ptr=&tloc;
    int result;
    rep->length = 0;
    pd->neerr = 0;
    pd->firstError = 0;
    pd->numRead = 0;
    P_io_getLocB (pads,loc_ptr,0);
    if (0==(rep->_internal)) 
      {
        rep->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(rep->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"entries_t_read","");
          }
      }
    if (0==(pd->_internal)) 
      {
        pd->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(pd->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"entries_t_read","");
          }
      }
    // Read input until we reach a termination condition
    if ((!P_PS_isPanic (pd))&&(!P_io_at_eof (pads))) 
      {
        {
          P_io_getLocB (pads,&(pd->loc),0);
          while (1)
            {
              // Ready to read next element
              (rep->length)++;
              if (0!=RBuf_reserve (rep->_internal,(void **) (&(rep->elts)),sizeof(entry_t),rep->length,0)) 
                {
                  PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"entries_t_read",0);
                }
              if (0!=RBuf_reserve (pd->_internal,(void **) (&(pd->elts)),sizeof(entry_t_pd),rep->length,0)) 
                {
                  PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"entries_t_read",0);
                }
              if ((rep->length)>1) 
                {
                  P_io_getLocB (pads,&((pd->elts)[rep->length].loc),0);
                  if (P_POS_EQ (((pd->elts)[(rep->length)-1].loc).b,((pd->elts)[rep->length].loc).b)) 
                    {
                      // array termination from lack of progress
                      (rep->length)-=2;
                      break;
                    }
                }
              result = entry_t_read (pads,&(m->element),&(pd->elts)[(rep->length)-1],&(rep->elts)[(rep->length)-1]);
              (pd->numRead)++;
              if (result==P_ERR) 
                {
                  // in markErrorSs
                  if (P_Test_NotIgnore (m->compoundLevel)) 
                    {
                      (pd->neerr)++;
                      if (!(pd->nerr)) 
                        {
                          (pd->nerr)++;
                          pd->errCode = P_ARRAY_ELEM_ERR;
                          P_io_getLocE (pads,&(pd->loc),-1);
                          // Index of first element with an error
                          pd->firstError = ((rep->length)-1);
                        }
                      if (P_spec_level (pads)) 
                        return P_ERR;
                    }
                }
              if (P_PS_isPanic (&(pd->elts)[(rep->length)-1])) 
                {
                  {
                    // No recovery possible
                    P_PS_setPanic (pd);
                    break;
                  }
                }
              // Have we finished reading array?
              if (P_io_at_eof (pads)||P_io_at_eor (pads)) 
                {
                  break;
                }
            }
        }
      }
    pd->length = (rep->length);
    return ((pd->nerr)==0) ? P_OK : P_ERR;
  }
}
void entries_t_ro_params_init (entries_t_ro_params_t *params)
{
}
Pread_res_t entries_t_final_checks (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr,int consume)
{
  PDCI_IODISC_3P_CHECKS ("entries_t_final_checks",m,pd,rep);
  {
    PCGEN_ARRAY_UNSET_PARTIAL ();
    pd->length = (rep->length);
    return PCGEN_ARRAY_RET_DONE (consume);
  }
}
Pread_res_t entries_t_read_one_init (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr)
{
  PDCI_IODISC_3P_CHECKS ("entries_t_read_one_init",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    rep->length = 0;
    pd->neerr = 0;
    pd->firstError = 0;
    pd->numRead = 0;
    P_io_getLocB (pads,loc_ptr,0);
    if (0==(rep->_internal)) 
      {
        rep->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(rep->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"entries_t_read","");
          }
      }
    if (0==(pd->_internal)) 
      {
        pd->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(pd->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"entries_t_read","");
          }
      }
    if (P_PS_isPanic (pd)||P_io_at_eof (pads)) 
      PCGEN_ARRAY_DO_FINAL_CHECKS ();
    P_io_getLocB (pads,&(pd->loc),0);
    PCGEN_ARRAY_SET_PARTIAL ();
    return PCGEN_ARRAY_RET_ONGOING (0);
    PCGEN_ARRAY_LBL_FINAL_CHECKS ();
    return entries_t_final_checks (pads,m,pd,rep,loc_ptr,0);
  }
}
Pread_res_t entries_t_read_one (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr,entry_t_pd *elt_pd,entry_t *elt_rep)
{
  PDCI_IODISC_3P_CHECKS ("entries_t_read_one",m,pd,rep);
  PDCI_IODISC_2P_CHECKS ("entries_t_read_one",elt_pd,elt_rep);
  {
    int haveData=0;
    PCGEN_ARRAY_RO_DECS ();
    PCGEN_ARRAY_TEST_ALREADY_DONE ();
    PCGEN_ARRAY_GET_BEGIN_LOC ();
    // Ready to read next element
    if ((pd->numRead)>0) 
      {
      }
    PCGEN_ARRAY_READ_ELEM_HD (entry_t_read (pads,&(m->element),elt_pd,elt_rep),haveData);
    PCGEN_ARRAY_TEST_READ_ERR (1,1);
    if (P_PS_isPanic (elt_pd)) 
      {
        {
          // No recovery possible
          P_PS_setPanic (pd);
          PCGEN_ARRAY_DO_FINAL_CHECKS ();
        }
      }
    // Have we finished reading array?
    if (P_io_at_eof (pads)||P_io_at_eor (pads)) 
      {
        PCGEN_ARRAY_DO_FINAL_CHECKS ();
      }
    PCGEN_ARRAY_TEST_FC_SOURCE_ADVANCE2 ();
    return PCGEN_ARRAY_RET_ONGOING (1);
    PCGEN_ARRAY_LBL_FINAL_CHECKS ();
    return entries_t_final_checks (pads,m,pd,rep,loc_ptr,haveData);
  }
}
Perror_t entries_t_read (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep)
{
  PDCI_IODISC_3P_CHECKS ("entries_t_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  {
    Ploc_t tloc;
    Ploc_t *loc_ptr=&tloc;
    int i=0;
    int result;
    entries_t_read_one_init (pads,m,pd,rep,loc_ptr);
    PCGEN_ARRAY_READ_ALL (PCGEN_ARRAY_RESERVE_SPACE (entries_t,entry_t,entry_t_pd,0),entries_t_read_one (pads,m,pd,rep,loc_ptr,&(pd->elts)[i],&(rep->elts)[i]),i = (rep->length),"entries_t_read");
    return PCGEN_ARRAY_STD_RETURN ();
  }
}
Pread_res_t entries_t_reread_one (P_t *pads,entries_t_m *m,entries_t_pd *pd,entries_t *rep,Ploc_t *loc_ptr,entry_t_pd *elt_pd,entry_t *elt_rep,int notFirstElt)
{
  PDCI_IODISC_3P_CHECKS ("entries_t_reread_one",m,pd,rep);
  PDCI_IODISC_2P_CHECKS ("entries_t_reread_one",elt_pd,elt_rep);
  {
    // Ready to read element
    PCGEN_ARRAY_REREAD_ELEM_BODY (entry_t_read (pads,&(m->element),elt_pd,elt_rep));
    return PCGEN_ARRAY_REREAD_ELEM_RET ();
  }
}
int is_entries_t (entries_t *rep)
{
  int violated=0;
  {
    int i;
    for (i = 0; (!violated)&&(i<(rep->length)); i++)
      {
        if (!is_entry_t (&(rep->elts)[i])) 
          {
            violated = 1;
          }
      }
  }
  return !violated;
}
Perror_t entries_t_acc_init (P_t *pads,entries_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->length))) 
    {
      nerr++;
    }
  if (P_ERR==entry_t_acc_init (pads,&(acc->compoundLevel))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        if (P_ERR==entry_t_acc_init (pads,&(acc->arrayDetail)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t entries_t_acc_reset (P_t *pads,entries_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->length))) 
    {
      nerr++;
    }
  if (P_ERR==entry_t_acc_reset (pads,&(acc->compoundLevel))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        if (P_ERR==entry_t_acc_reset (pads,&(acc->arrayDetail)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t entries_t_acc_cleanup (P_t *pads,entries_t_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->length))) 
    {
      nerr++;
    }
  if (P_ERR==entry_t_acc_cleanup (pads,&(acc->compoundLevel))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        if (P_ERR==entry_t_acc_cleanup (pads,&(acc->arrayDetail)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t entries_t_acc_add (P_t *pads,entries_t_acc *acc,entries_t_pd *pd,entries_t *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = (pd->errCode);
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==Puint32_acc_add (pads,&(acc->length),&tpd,&(rep->length))) 
        {
          nerr++;
        }
      {
        int i;
        for (i = 0; i<(rep->length); i++)
          {
            if (i<10) 
              {
                if (P_ERR==entry_t_acc_add (pads,&(acc->arrayDetail)[i],&(pd->elts)[i],&(rep->elts)[i])) 
                  {
                    nerr++;
                  }
              }
            if (P_ERR==entry_t_acc_add (pads,&(acc->compoundLevel),&(pd->elts)[i],&(rep->elts)[i])) 
              {
                nerr++;
              }
          }
      }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t entries_t_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,entries_t_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "array entries_t of entry_t";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_ARRAY_ACC_REP_NOVALS ();
  if (P_ERR==Puint32_acc_report2io (pads,outstr,"Array lengths","array length",-1,&(acc->length))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.allArrayElts",prefix);
  if (P_ERR==entry_t_acc_report2io (pads,outstr,sfstruse (tmpstr),"all array element",nst,&(acc->compoundLevel))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  {
    int i;
    for (i = 0; i<((((acc->length).max)<10) ? (acc->length).max : 10); i++)
      {
        sfprintf (tmpstr,"%s.arrayDetail[%d]",prefix,i);
        if (P_ERR==entry_t_acc_report2io (pads,outstr,sfstruse (tmpstr),"array element",nst,&(acc->arrayDetail)[i])) 
          {
            sfstrclose (tmpstr);
            return P_ERR;
          }
      }
  }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t entries_t_acc_report (P_t *pads,char const *prefix,char const *what,int nst,entries_t_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = entries_t_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t entries_t_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entries_t_pd *pd,entries_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("entries_t_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  {
    int i=0;
    if ((rep->length)>1) 
      {
        for (i = 0; i<((rep->length)-1); i++)
          {
            tlen_PCGEN_ = entry_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->elts)[i],&(rep->elts)[i]);
            PCGEN_TLEN_UPDATES ();
          }
      }
    if ((rep->length)!=0) 
      {
        tlen_PCGEN_ = entry_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->elts)[i],&(rep->elts)[i]);
        PCGEN_TLEN_UPDATES ();
      }
  }
  return length_PCGEN_;
}
ssize_t entries_t_write2io (P_t *pads,Sfio_t *io,entries_t_pd *pd,entries_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("entries_t_write2io",entries_t_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t entries_t_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,entries_t_pd *pd,entries_t *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("entries_t_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("entries_t");
  PCGEN_ARRAY_PD_XML_OUT ();
  {
    int i=0;
    for (i = 0; i<(rep->length); i++)
      {
        tlen_PCGEN_ = entry_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->elts)[i],&(rep->elts)[i],"elt",indent+2);
        PCGEN_TLEN_UPDATES ();
      }
  }
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t entries_t_write_xml_2io (P_t *pads,Sfio_t *io,entries_t_pd *pd,entries_t *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("entries_t_write_xml_2io",entries_t_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t entries_t_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,entries_t_m *m,entries_t_pd *pd,entries_t *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  int i=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("entries_t_fmt2buf_final");
  PCGEN_FMT2BUF_ARRAY ("entries_t_fmt2buf_final",entry_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->element),&(pd->elts)[i],&(rep->elts)[i]));
  PCGEN_FMT2BUF_FIX_LAST ();
  return length_PCGEN_;
}
ssize_t entries_t_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,entries_t_m *m,entries_t_pd *pd,entries_t *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("entries_t_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"entries_t"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return entries_t_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t entries_t_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,entries_t_m *m,entries_t_pd *pd,entries_t *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("entries_t_fmt2io",entries_t_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
Perror_t out_sum_init (P_t *pads,out_sum *rep)
{
  PDCI_DISC_1P_CHECKS ("out_sum_init",rep);
  memset ((void *) rep,0,sizeof(out_sum));
  return P_OK;
}
Perror_t out_sum_pd_init (P_t *pads,out_sum_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("out_sum_pd_init",pd);
  memset ((void *) pd,0,sizeof(out_sum_pd));
  return P_OK;
}
Perror_t out_sum_cleanup (P_t *pads,out_sum *rep)
{
  PDCI_DISC_1P_CHECKS ("out_sum_cleanup",rep);
  entries_t_cleanup (pads,&(rep->es));
  return P_OK;
}
Perror_t out_sum_pd_cleanup (P_t *pads,out_sum_pd *pd)
{
  PDCI_DISC_1P_CHECKS ("out_sum_pd_cleanup",pd);
  entries_t_pd_cleanup (pads,&(pd->es));
  return P_OK;
}
Perror_t out_sum_copy (P_t *pads,out_sum *rep_dst,out_sum *rep_src)
{
  PDCI_DISC_2P_CHECKS ("out_sum_copy",rep_src,rep_dst);
  memcpy ((void *) (&(rep_dst->h)),(void *) (&(rep_src->h)),sizeof(summary_header_t));
  entries_t_copy (pads,&(rep_dst->es),&(rep_src->es));
  return P_OK;
}
Perror_t out_sum_pd_copy (P_t *pads,out_sum_pd *pd_dst,out_sum_pd *pd_src)
{
  PDCI_DISC_2P_CHECKS ("out_sum_pd_copy",pd_src,pd_dst);
  memcpy ((void *) (&(pd_dst->h)),(void *) (&(pd_src->h)),sizeof(summary_header_t_pd));
  entries_t_pd_copy (pads,&(pd_dst->es),&(pd_src->es));
  return P_OK;
}
void out_sum_m_init (P_t *pads,out_sum_m *mask,Pbase_m baseMask)
{
  PDCI_fill_mask ((Pbase_m *) mask,baseMask,sizeof(out_sum_m));
}
Perror_t out_sum_read (P_t *pads,out_sum_m *m,out_sum_pd *pd,out_sum *rep)
{
  PDCI_IODISC_3P_CHECKS ("out_sum_read",m,pd,rep);
  PD_COMMON_INIT_NO_ERR (pd);
  PD_COMMON_READ_INIT (pads,pd);
  // Read field 'h'
  PCGEN_STRUCT_READ_FIRST ("out_sum_read",h,summary_header_t_read (pads,&(m->h),&(pd->h),&(rep->h)),_NOOP);
  // Read field 'es'
  PCGEN_STRUCT_READ_NEXT ("out_sum_read",es,entries_t_read (pads,&(m->es),&(pd->es),&(rep->es)),_NOOP);
  return ((pd->nerr)==0) ? P_OK : P_ERR;
}
int is_out_sum (out_sum *rep)
{
  return is_summary_header_t (&(rep->h))&&(is_entries_t (&(rep->es))&&1);
}
Perror_t out_sum_acc_init (P_t *pads,out_sum_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==summary_header_t_acc_init (pads,&(acc->h))) 
    {
      nerr++;
    }
  if (P_ERR==entries_t_acc_init (pads,&(acc->es))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_acc_reset (P_t *pads,out_sum_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==summary_header_t_acc_reset (pads,&(acc->h))) 
    {
      nerr++;
    }
  if (P_ERR==entries_t_acc_reset (pads,&(acc->es))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_acc_cleanup (P_t *pads,out_sum_acc *acc)
{
  Puint32 nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->nerr))) 
    {
      nerr++;
    }
  if (P_ERR==summary_header_t_acc_cleanup (pads,&(acc->h))) 
    {
      nerr++;
    }
  if (P_ERR==entries_t_acc_cleanup (pads,&(acc->es))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_acc_add (P_t *pads,out_sum_acc *acc,out_sum_pd *pd,out_sum *rep)
{
  Puint32 nerr=0;
  Pbase_pd tpd;
  tpd.errCode = P_NO_ERR;
  if (P_ERR==Puint32_acc_add (pads,&(acc->nerr),&tpd,&(pd->nerr))) 
    {
      nerr++;
    }
  if ((pd->errCode)!=P_PANIC_SKIPPED) 
    {
      if (P_ERR==summary_header_t_acc_add (pads,&(acc->h),&(pd->h),&(rep->h))) 
        {
          nerr++;
        }
      if (P_ERR==entries_t_acc_add (pads,&(acc->es),&(pd->es),&(rep->es))) 
        {
          nerr++;
        }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_acc_report2io (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,out_sum_acc *acc)
{
  Sfio_t *tmpstr;
  if (!(tmpstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if ((!prefix)||(0==(*prefix))) 
    {
      prefix = "<top>";
    }
  if (!what) 
    {
      what = "struct out_sum";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what,0);
  PCGEN_STRUCT_ACC_REP_NOVALS ();
  if (P_ERR==P_nerr_acc_report2io (pads,outstr,"Errors","errors",-1,&(acc->nerr))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.h",prefix);
  if (P_ERR==summary_header_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->h))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.es",prefix);
  if (P_ERR==entries_t_acc_report2io (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->es))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t out_sum_acc_report (P_t *pads,char const *prefix,char const *what,int nst,out_sum_acc *acc)
{
  Perror_t result;
  Sfio_t *outstr;
  if (!(outstr = sfstropen ())) 
    {
      return P_ERR;
    }
  if (((!pads)||(!acc))||(!(pads->disc))) 
    {
      return P_ERR;
    }
  if (!((pads->disc)->error_fn)) 
    {
      return P_OK;
    }
  result = out_sum_acc_report2io (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->error_fn) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
ssize_t out_sum_write2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,out_sum_pd *pd,out_sum *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("out_sum_write2buf",buf,buf_full,rep);
  *buf_full = 0;
  tlen_PCGEN_ = summary_header_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->h),&(rep->h));
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = entries_t_write2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->es),&(rep->es));
  PCGEN_FINAL_TLEN_UPDATES ();
  return length_PCGEN_;
}
ssize_t out_sum_write2io (P_t *pads,Sfio_t *io,out_sum_pd *pd,out_sum *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("out_sum_write2io",out_sum_write2buf (pads,buf,buf_len,&buf_full,pd,rep));
  return -1;
}
ssize_t out_sum_write_xml_2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,out_sum_pd *pd,out_sum *rep,char const *tag,int indent)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  PDCI_IODISC_3P_CHECKS_RET_SSIZE ("out_sum_write_xml_2buf",buf,buf_full,rep);
  *buf_full = 0;
  PCGEN_TAG_OPEN_XML_OUT ("out_sum");
  PCGEN_STRUCT_PD_XML_OUT ();
  tlen_PCGEN_ = summary_header_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->h),&(rep->h),"h",indent+2);
  PCGEN_TLEN_UPDATES ();
  tlen_PCGEN_ = entries_t_write_xml_2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&(pd->es),&(rep->es),"es",indent+2);
  PCGEN_TLEN_UPDATES ();
  PCGEN_TAG_CLOSE_XML_OUT ();
  return length_PCGEN_;
}
ssize_t out_sum_write_xml_2io (P_t *pads,Sfio_t *io,out_sum_pd *pd,out_sum *rep,char const *tag,int indent)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("out_sum_write_xml_2io",out_sum_write_xml_2buf (pads,buf,buf_len,&buf_full,pd,rep,tag,indent));
  return -1;
}
ssize_t out_sum_fmt2buf_final (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,out_sum_m *m,out_sum_pd *pd,out_sum *rep)
{
  Pbyte *buf_cursor_PCGEN_=buf;
  ssize_t length_PCGEN_=0;
  ssize_t tlen_PCGEN_;
  char const *tdelim_PCGEN_;
  int trequestedOut_PCGEN_=0;
  PCGEN_STRUCT_FMT2BUF_FINAL_INIT ("out_sum_fmt2buf_final");
  PCGEN_FMT2BUF_STRUCT_FIELD ("summary_header_t_fmt2buf",summary_header_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->h),&(pd->h),&(rep->h)));
  PCGEN_FMT2BUF_STRUCT_FIELD ("entries_t_fmt2buf",entries_t_fmt2buf (pads,buf_cursor_PCGEN_,buf_len,buf_full,&trequestedOut_PCGEN_,tdelim_PCGEN_,&(m->es),&(pd->es),&(rep->es)));
  PCGEN_FMT2BUF_FIX_LAST ();
  return length_PCGEN_;
}
ssize_t out_sum_fmt2buf (P_t *pads,Pbyte *buf,size_t buf_len,int *buf_full,int *requestedOut,char const *delims,out_sum_m *m,out_sum_pd *pd,out_sum *rep)
{
  Pfmt_fn fn_PCGEN_;
  PCGEN_STANDARD_FMT2BUF_INIT ("out_sum_fmt2buf",fn_PCGEN_ = PDCI_GET_FMT_FN (pads,"out_sum"),P_invoke_fmt_fn (fn_PCGEN_,pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep));
  return out_sum_fmt2buf_final (pads,buf,buf_len,buf_full,requestedOut,delims,m,pd,rep);
}
ssize_t out_sum_fmt2io (P_t *pads,Sfio_t *io,int *requestedOut,char const *delims,out_sum_m *m,out_sum_pd *pd,out_sum *rep)
{
  Pbyte *buf;
  int buf_full;
  size_t buf_len;
  PCGEN_WRITE2IO_USE_WRITE2BUF ("out_sum_fmt2io",out_sum_fmt2buf (pads,buf,buf_len,&buf_full,requestedOut,delims,m,pd,rep));
  return -1;
}
void P_lib_init ()
{
  // Initialize character classes
}

