#include "pads-internal.h"
#include "dibbler2.h"
Perror_t auint32_vbar_read_internal (P_t *pads,auint32_vbar_em *modem,auint32_vbar_ed *moded,auint32_vbar *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  /* XXX_OPT ************ Check short-circuit Ignore case ************* */
  if (modem->val == P_Ignore) { /* all fields prior to delim are set to Ignore */
    if (P_ERR!=Pa_char_lit_scan (pads,124,124,1,0,0)) 
      {
	moded->errCode = P_NO_ERR;
	return P_OK;
      }
    /* no vbar -- panic */
    if (P_spec_level (pads)) 
      {
	return P_ERR;
      }
    P_io_getLoc (pads,&(moded->loc),0);
    PDCI_report_err (pads,P_LEV_INFO,&(moded->loc),P_MISSING_LITERAL,"Using scan (due to P_Ignore), cannot find separator (%s) forward of current IO loc.","|");
    moded->errCode = P_MISSING_LITERAL;
    moded->nerr = 1;
    moded->panic = 1;
    return P_ERR;
  }
  /* ************ Reading field: val. ************* */
  /* XXX_OPT: NOT NEEDED */
#if 0
  if (moded->panic) 
    {
      (moded->val).panic = 1;
      (moded->val).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->val).loc),0);
      (moded->nerr)+=1;
    }
  else
#endif
    {
      if (P_ERR==Pa_uint32_read_internal (pads,&(modem->val),&(moded->val),&(modrep->val))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->val).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->val).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  {
    /* ********* Reading delimiter field: | ********* */
    {
      Pbase_ed ted;
      if (moded->panic) 
        {
          size_t n;
          if (P_ERR!=Pa_char_lit_scan (pads,124,124,1,0,&n)) 
            {
              moded->panic = 0;
            }
        }
      else
        {
          Pbase_em tem=P_Check;
          if (P_ERR==Pchar_lit_read_internal (pads,&tem,&ted,124)) 
            {
              if (P_spec_level (pads)) 
                {
                  return P_ERR;
                }
              PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_MISSING_LITERAL,"Missing separator: %s.","|");
              if (0==(moded->nerr)) 
                {
                  moded->errCode = P_MISSING_LITERAL;
                  moded->loc = (ted.loc);
                }
              (moded->nerr)+=1;
              moded->panic = 1;
            }
        }
    }
  }
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t auint32_vbar_read (P_t *pads,auint32_vbar_em *em,auint32_vbar_ed *ed,auint32_vbar *rep)
{
  auint32_vbar tmprep={0};
  auint32_vbar *modrep=rep;
  auint32_vbar_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),(enum Pbase_em_e) 0};
  auint32_vbar_em *modem=em;
  auint32_vbar_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}};
  auint32_vbar_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"auint32_vbar_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"auint32_vbar_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"auint32_vbar_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"auint32_vbar_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return auint32_vbar_read_internal (pads,modem,moded,modrep);
}
Perror_t auint32_vbar_acc_init (P_t *pads,auint32_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->val))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t auint32_vbar_acc_reset (P_t *pads,auint32_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->val))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t auint32_vbar_acc_cleanup (P_t *pads,auint32_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->val))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t auint32_vbar_acc_add (P_t *pads,auint32_vbar_acc *acc,auint32_vbar_ed *ed,auint32_vbar *rep)
{
  int nerr=0;
  if (P_ERR==Puint32_acc_add (pads,&(acc->val),&(ed->val),&(rep->val))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t auint32_vbar_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,auint32_vbar_acc *acc)
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
      what = "struct auint32_vbar";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.val",prefix);
  if (P_ERR==Puint32_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->val))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t auint32_vbar_acc_report (P_t *pads,char const *prefix,char const *what,int nst,auint32_vbar_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = auint32_vbar_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t auint64_vbar_read_internal (P_t *pads,auint64_vbar_em *modem,auint64_vbar_ed *moded,auint64_vbar *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  /* XXX_OPT ************ Check short-circuit Ignore case ************* */
  if (modem->val == P_Ignore) { /* all fields prior to delim are set to Ignore */
    if (P_ERR!=Pa_char_lit_scan (pads,124,124,1,0,0)) 
      {
	moded->errCode = P_NO_ERR;
	return P_OK;
      }
    /* no vbar -- panic */
    if (P_spec_level (pads)) 
      {
	return P_ERR;
      }
    P_io_getLoc (pads,&(moded->loc),0);
    PDCI_report_err (pads,P_LEV_INFO,&(moded->loc),P_MISSING_LITERAL,"Using scan (due to P_Ignore), cannot find separator (%s) forward of current IO loc.","|");
    moded->errCode = P_MISSING_LITERAL;
    moded->nerr = 1;
    moded->panic = 1;
    return P_ERR;
  }
  /* ************ Reading field: val. ************* */
  /* XXX_OPT : NOT NEEDED */
#if 0
  if (moded->panic) 
    {
      (moded->val).panic = 1;
      (moded->val).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->val).loc),0);
      (moded->nerr)+=1;
    }
  else
#endif
    {
      if (P_ERR==Pa_uint64_read_internal (pads,&(modem->val),&(moded->val),&(modrep->val))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->val).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->val).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  {
    /* ********* Reading delimiter field: | ********* */
    {
      Pbase_ed ted;
      if (moded->panic) 
        {
          size_t n;
          if (P_ERR!=Pa_char_lit_scan (pads,124,124,1,0,&n)) 
            {
              moded->panic = 0;
            }
        }
      else
        {
          Pbase_em tem=P_Check;
          if (P_ERR==Pchar_lit_read_internal (pads,&tem,&ted,124)) 
            {
              if (P_spec_level (pads)) 
                {
                  return P_ERR;
                }
              PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_MISSING_LITERAL,"Missing separator: %s.","|");
              if (0==(moded->nerr)) 
                {
                  moded->errCode = P_MISSING_LITERAL;
                  moded->loc = (ted.loc);
                }
              (moded->nerr)+=1;
              moded->panic = 1;
            }
        }
    }
  }
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t auint64_vbar_read (P_t *pads,auint64_vbar_em *em,auint64_vbar_ed *ed,auint64_vbar *rep)
{
  auint64_vbar tmprep={0};
  auint64_vbar *modrep=rep;
  auint64_vbar_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),(enum Pbase_em_e) 0};
  auint64_vbar_em *modem=em;
  auint64_vbar_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}};
  auint64_vbar_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"auint64_vbar_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"auint64_vbar_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"auint64_vbar_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"auint64_vbar_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return auint64_vbar_read_internal (pads,modem,moded,modrep);
}
Perror_t auint64_vbar_acc_init (P_t *pads,auint64_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Puint64_acc_init (pads,&(acc->val))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t auint64_vbar_acc_reset (P_t *pads,auint64_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Puint64_acc_reset (pads,&(acc->val))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t auint64_vbar_acc_cleanup (P_t *pads,auint64_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Puint64_acc_cleanup (pads,&(acc->val))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t auint64_vbar_acc_add (P_t *pads,auint64_vbar_acc *acc,auint64_vbar_ed *ed,auint64_vbar *rep)
{
  int nerr=0;
  if (P_ERR==Puint64_acc_add (pads,&(acc->val),&(ed->val),&(rep->val))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t auint64_vbar_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,auint64_vbar_acc *acc)
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
      what = "struct auint64_vbar";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.val",prefix);
  if (P_ERR==Puint64_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->val))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t auint64_vbar_acc_report (P_t *pads,char const *prefix,char const *what,int nst,auint64_vbar_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = auint64_vbar_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t just_vbar_read_internal (P_t *pads,just_vbar_em *modem,just_vbar_ed *moded,just_vbar *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  {
    /* ********* Reading delimiter field: | ********* */
    {
      Pbase_ed ted;
  /* XXX_OPT: NOT NEEDED */
#if 0
      if (moded->panic) 
        {
          size_t n;
          if (P_ERR!=Pa_char_lit_scan (pads,124,124,1,0,&n)) 
            {
              moded->panic = 0;
            }
        }
      else
#endif
        {
          Pbase_em tem=P_Check;
          if (P_ERR==Pchar_lit_read_internal (pads,&tem,&ted,124)) 
            {
              if (P_spec_level (pads)) 
                {
                  return P_ERR;
                }
              PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_MISSING_LITERAL,"Missing separator: %s.","|");
              if (0==(moded->nerr)) 
                {
                  moded->errCode = P_MISSING_LITERAL;
                  moded->loc = (ted.loc);
                }
              (moded->nerr)+=1;
              moded->panic = 1;
            }
        }
    }
  }
  /* ************* Reading field: d. ************** */
  /* XXX_OPT: DO NOT DO DUMMY STUFF */
#if 0
  if (moded->panic) 
    {
      (moded->d).panic = 1;
      (moded->d).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->d).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==Pdummy_read_internal (pads,&(modem->d),0,&(moded->d),&(modrep->d))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->d).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->d).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
#endif
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t just_vbar_read (P_t *pads,just_vbar_em *em,just_vbar_ed *ed,just_vbar *rep)
{
  just_vbar tmprep={0};
  just_vbar *modrep=rep;
  just_vbar_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),(enum Pbase_em_e) 0};
  just_vbar_em *modem=em;
  just_vbar_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}};
  just_vbar_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"just_vbar_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"just_vbar_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"just_vbar_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"just_vbar_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return just_vbar_read_internal (pads,modem,moded,modrep);
}
Perror_t just_vbar_acc_init (P_t *pads,just_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->d))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t just_vbar_acc_reset (P_t *pads,just_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->d))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t just_vbar_acc_cleanup (P_t *pads,just_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->d))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t just_vbar_acc_add (P_t *pads,just_vbar_acc *acc,just_vbar_ed *ed,just_vbar *rep)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_add (pads,&(acc->d),&(ed->d),&(rep->d))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t just_vbar_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,just_vbar_acc *acc)
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
      what = "struct just_vbar";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.d",prefix);
  if (P_ERR==Pint32_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->d))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t just_vbar_acc_report (P_t *pads,char const *prefix,char const *what,int nst,just_vbar_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = just_vbar_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
char const *opt_auint32_vbar_tag2str (opt_auint32_vbar_tag which)
{
  switch (which)
    {
      
    case 1: 
      return "yes32";
      
    case 2: 
      return "no32";
      
    default: 
      return "* unknown meth *";
    }
}
Perror_t opt_auint32_vbar_read_internal (P_t *pads,opt_auint32_vbar_em *modem,opt_auint32_vbar_ed *moded,opt_auint32_vbar *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  /* ************ Reading field: yes32 ************ */
  if (P_ERR==P_io_checkpoint (pads,1)) 
    {
      PDCI_report_err (pads,P_LEV_FATAL,0,P_CHKPOINT_ERR,0);
    }
  modrep->tag = yes32;
  if (P_ERR==auint32_vbar_read_internal (pads,&(modem->yes32),&(moded->yes32),&((modrep->val).yes32))) 
    {
      if (P_ERR==P_io_restore (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_RESTORE_ERR,0);
        }
    }
  else
    {
      if (P_ERR==P_io_commit (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_COMMIT_ERR,0);
        }
      return P_OK;
    }
  /* ************ Reading field: no32 ************* */
  if (P_ERR==P_io_checkpoint (pads,1)) 
    {
      PDCI_report_err (pads,P_LEV_FATAL,0,P_CHKPOINT_ERR,0);
    }
  modrep->tag = no32;
  if (P_ERR==just_vbar_read_internal (pads,&(modem->no32),&(moded->no32),&((modrep->val).no32))) 
    {
      if (P_ERR==P_io_restore (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_RESTORE_ERR,0);
        }
    }
  else
    {
      if (P_ERR==P_io_commit (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_COMMIT_ERR,0);
        }
      return P_OK;
    }
  /* ********* We didn't match any branch ********* */
  (moded->nerr)++;
  moded->errCode = P_UNION_MATCH_ERR;
  P_io_getLoc (pads,&(moded->loc),0);
  PDCI_report_err (pads,P_LEV_INFO,&(moded->loc),moded->errCode,"Did not match any branch of union opt_auint32_vbar.");
  moded->panic = 1;
  return P_ERR;
}
Perror_t opt_auint32_vbar_read (P_t *pads,opt_auint32_vbar_em *em,opt_auint32_vbar_ed *ed,opt_auint32_vbar *rep)
{
  opt_auint32_vbar tmprep={(enum opt_auint32_vbar_tag_e) 0,{{0}}};
  opt_auint32_vbar *modrep=rep;
  opt_auint32_vbar_em tmpem={{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}};
  opt_auint32_vbar_em *modem=em;
  opt_auint32_vbar_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}};
  opt_auint32_vbar_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"opt_auint32_vbar_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"opt_auint32_vbar_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"opt_auint32_vbar_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"opt_auint32_vbar_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return opt_auint32_vbar_read_internal (pads,modem,moded,modrep);
}
Perror_t opt_auint32_vbar_acc_init (P_t *pads,opt_auint32_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_init (pads,&(acc->yes32))) 
    {
      nerr++;
    }
  if (P_ERR==just_vbar_acc_init (pads,&(acc->no32))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t opt_auint32_vbar_acc_reset (P_t *pads,opt_auint32_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_reset (pads,&(acc->yes32))) 
    {
      nerr++;
    }
  if (P_ERR==just_vbar_acc_reset (pads,&(acc->no32))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t opt_auint32_vbar_acc_cleanup (P_t *pads,opt_auint32_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_cleanup (pads,&(acc->yes32))) 
    {
      nerr++;
    }
  if (P_ERR==just_vbar_acc_cleanup (pads,&(acc->no32))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t opt_auint32_vbar_acc_add (P_t *pads,opt_auint32_vbar_acc *acc,opt_auint32_vbar_ed *ed,opt_auint32_vbar *rep)
{
  int nerr=0;
  Pbase_ed ted;
  ted.errCode = P_NO_ERR;
  if (P_ERR==Pint32_acc_add (pads,&(acc->tag),&ted,(Pint32 *) (&(rep->tag)))) 
    {
      nerr++;
    }
  switch (rep->tag)
    {
      
    case 1: 
      {
        if (P_ERR==auint32_vbar_acc_add (pads,&(acc->yes32),&(ed->yes32),&((rep->val).yes32))) 
          {
            nerr++;
          }
        break;
      }
      
    case 2: 
      {
        if (P_ERR==just_vbar_acc_add (pads,&(acc->no32),&(ed->no32),&((rep->val).no32))) 
          {
            nerr++;
          }
        break;
      }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t opt_auint32_vbar_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,opt_auint32_vbar_acc *acc)
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
      what = "union opt_auint32_vbar";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  if (P_ERR==Pint32_acc_report_map_internal (pads,outstr,"Union tag","tag",-1,(Pint32_map_fn) opt_auint32_vbar_tag2str,&(acc->tag))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each tag arm of %s]\n",prefix);
  sfprintf (tmpstr,"%s.yes32",prefix);
  if (P_ERR==auint32_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->yes32))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.no32",prefix);
  if (P_ERR==just_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->no32))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t opt_auint32_vbar_acc_report (P_t *pads,char const *prefix,char const *what,int nst,opt_auint32_vbar_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = opt_auint32_vbar_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
char const *opt_auint64_vbar_tag2str (opt_auint64_vbar_tag which)
{
  switch (which)
    {
      
    case 1: 
      return "yes64";
      
    case 2: 
      return "no64";
      
    default: 
      return "* unknown meth *";
    }
}
Perror_t opt_auint64_vbar_read_internal (P_t *pads,opt_auint64_vbar_em *modem,opt_auint64_vbar_ed *moded,opt_auint64_vbar *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  /* ************ Reading field: yes64 ************ */
  if (P_ERR==P_io_checkpoint (pads,1)) 
    {
      PDCI_report_err (pads,P_LEV_FATAL,0,P_CHKPOINT_ERR,0);
    }
  modrep->tag = yes64;
  if (P_ERR==auint64_vbar_read_internal (pads,&(modem->yes64),&(moded->yes64),&((modrep->val).yes64))) 
    {
      if (P_ERR==P_io_restore (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_RESTORE_ERR,0);
        }
    }
  else
    {
      if (P_ERR==P_io_commit (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_COMMIT_ERR,0);
        }
      return P_OK;
    }
  /* ************ Reading field: no64 ************* */
  if (P_ERR==P_io_checkpoint (pads,1)) 
    {
      PDCI_report_err (pads,P_LEV_FATAL,0,P_CHKPOINT_ERR,0);
    }
  modrep->tag = no64;
  if (P_ERR==just_vbar_read_internal (pads,&(modem->no64),&(moded->no64),&((modrep->val).no64))) 
    {
      if (P_ERR==P_io_restore (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_RESTORE_ERR,0);
        }
    }
  else
    {
      if (P_ERR==P_io_commit (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_COMMIT_ERR,0);
        }
      return P_OK;
    }
  /* ********* We didn't match any branch ********* */
  (moded->nerr)++;
  moded->errCode = P_UNION_MATCH_ERR;
  P_io_getLoc (pads,&(moded->loc),0);
  PDCI_report_err (pads,P_LEV_INFO,&(moded->loc),moded->errCode,"Did not match any branch of union opt_auint64_vbar.");
  moded->panic = 1;
  return P_ERR;
}
Perror_t opt_auint64_vbar_read (P_t *pads,opt_auint64_vbar_em *em,opt_auint64_vbar_ed *ed,opt_auint64_vbar *rep)
{
  opt_auint64_vbar tmprep={(enum opt_auint64_vbar_tag_e) 0,{{0}}};
  opt_auint64_vbar *modrep=rep;
  opt_auint64_vbar_em tmpem={{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}};
  opt_auint64_vbar_em *modem=em;
  opt_auint64_vbar_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}};
  opt_auint64_vbar_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"opt_auint64_vbar_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"opt_auint64_vbar_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"opt_auint64_vbar_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"opt_auint64_vbar_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return opt_auint64_vbar_read_internal (pads,modem,moded,modrep);
}
Perror_t opt_auint64_vbar_acc_init (P_t *pads,opt_auint64_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==auint64_vbar_acc_init (pads,&(acc->yes64))) 
    {
      nerr++;
    }
  if (P_ERR==just_vbar_acc_init (pads,&(acc->no64))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t opt_auint64_vbar_acc_reset (P_t *pads,opt_auint64_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==auint64_vbar_acc_reset (pads,&(acc->yes64))) 
    {
      nerr++;
    }
  if (P_ERR==just_vbar_acc_reset (pads,&(acc->no64))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t opt_auint64_vbar_acc_cleanup (P_t *pads,opt_auint64_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==auint64_vbar_acc_cleanup (pads,&(acc->yes64))) 
    {
      nerr++;
    }
  if (P_ERR==just_vbar_acc_cleanup (pads,&(acc->no64))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t opt_auint64_vbar_acc_add (P_t *pads,opt_auint64_vbar_acc *acc,opt_auint64_vbar_ed *ed,opt_auint64_vbar *rep)
{
  int nerr=0;
  Pbase_ed ted;
  ted.errCode = P_NO_ERR;
  if (P_ERR==Pint32_acc_add (pads,&(acc->tag),&ted,(Pint32 *) (&(rep->tag)))) 
    {
      nerr++;
    }
  switch (rep->tag)
    {
      
    case 1: 
      {
        if (P_ERR==auint64_vbar_acc_add (pads,&(acc->yes64),&(ed->yes64),&((rep->val).yes64))) 
          {
            nerr++;
          }
        break;
      }
      
    case 2: 
      {
        if (P_ERR==just_vbar_acc_add (pads,&(acc->no64),&(ed->no64),&((rep->val).no64))) 
          {
            nerr++;
          }
        break;
      }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t opt_auint64_vbar_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,opt_auint64_vbar_acc *acc)
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
      what = "union opt_auint64_vbar";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  if (P_ERR==Pint32_acc_report_map_internal (pads,outstr,"Union tag","tag",-1,(Pint32_map_fn) opt_auint64_vbar_tag2str,&(acc->tag))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each tag arm of %s]\n",prefix);
  sfprintf (tmpstr,"%s.yes64",prefix);
  if (P_ERR==auint64_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->yes64))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.no64",prefix);
  if (P_ERR==just_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->no64))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t opt_auint64_vbar_acc_report (P_t *pads,char const *prefix,char const *what,int nst,opt_auint64_vbar_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = opt_auint64_vbar_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t no_pn_vbar_read_internal (P_t *pads,no_pn_vbar_em *modem,no_pn_vbar_ed *moded,no_pn_vbar *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  {
    /* ***** Reading delimiter field: "no_TN|" ****** */
    {
      Pbase_ed ted;
      Pstring strlit={"no_TN|",0,0,0};
      strlit.len = 6;
  /* XXX_OPT: NOT NEEDED */
#if 0
      if (moded->panic) 
        {
          size_t n;
          if (P_ERR!=Pstr_lit_scan (pads,&strlit,&strlit,1,0,&n)) 
            {
              moded->panic = 0;
            }
        }
      else
#endif
        {
          Pbase_em tem=P_Check;
          if (P_ERR==Pstr_lit_read_internal (pads,&tem,&ted,&strlit)) 
            {
              if (P_spec_level (pads)) 
                {
                  return P_ERR;
                }
              PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_MISSING_LITERAL,"Missing separator: %s.","\"no_TN|\"");
              if (0==(moded->nerr)) 
                {
                  moded->errCode = P_MISSING_LITERAL;
                  moded->loc = (ted.loc);
                }
              (moded->nerr)+=1;
              moded->panic = 1;
            }
        }
    }
  }
  /* ************* Reading field: d. ************** */
  /* XXX_OPT: DO NOT DO DUMMY STUFF */
#if 0
  if (moded->panic) 
    {
      (moded->d).panic = 1;
      (moded->d).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->d).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==Pdummy_read_internal (pads,&(modem->d),0,&(moded->d),&(modrep->d))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->d).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->d).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
#endif
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t no_pn_vbar_read (P_t *pads,no_pn_vbar_em *em,no_pn_vbar_ed *ed,no_pn_vbar *rep)
{
  no_pn_vbar tmprep={0};
  no_pn_vbar *modrep=rep;
  no_pn_vbar_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),(enum Pbase_em_e) 0};
  no_pn_vbar_em *modem=em;
  no_pn_vbar_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}};
  no_pn_vbar_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"no_pn_vbar_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"no_pn_vbar_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"no_pn_vbar_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"no_pn_vbar_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return no_pn_vbar_read_internal (pads,modem,moded,modrep);
}
Perror_t no_pn_vbar_acc_init (P_t *pads,no_pn_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->d))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t no_pn_vbar_acc_reset (P_t *pads,no_pn_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->d))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t no_pn_vbar_acc_cleanup (P_t *pads,no_pn_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->d))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t no_pn_vbar_acc_add (P_t *pads,no_pn_vbar_acc *acc,no_pn_vbar_ed *ed,no_pn_vbar *rep)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_add (pads,&(acc->d),&(ed->d),&(rep->d))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t no_pn_vbar_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,no_pn_vbar_acc *acc)
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
      what = "struct no_pn_vbar";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.d",prefix);
  if (P_ERR==Pint32_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->d))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t no_pn_vbar_acc_report (P_t *pads,char const *prefix,char const *what,int nst,no_pn_vbar_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = no_pn_vbar_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
char const *dib_pn_vbar_tag2str (dib_pn_vbar_tag which)
{
  switch (which)
    {
      
    case 1: 
      return "yesPN";
      
    case 2: 
      return "noPN";
      
    default: 
      return "* unknown meth *";
    }
}
Perror_t dib_pn_vbar_read_internal (P_t *pads,dib_pn_vbar_em *modem,dib_pn_vbar_ed *moded,dib_pn_vbar *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  /* ************ Reading field: yesPN ************ */
  if (P_ERR==P_io_checkpoint (pads,1)) 
    {
      PDCI_report_err (pads,P_LEV_FATAL,0,P_CHKPOINT_ERR,0);
    }
  modrep->tag = yesPN;
  if (P_ERR==auint64_vbar_read_internal (pads,&(modem->yesPN),&(moded->yesPN),&((modrep->val).yesPN))) 
    {
      if (P_ERR==P_io_restore (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_RESTORE_ERR,0);
        }
    }
  else
    {
      if (P_ERR==P_io_commit (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_COMMIT_ERR,0);
        }
      return P_OK;
    }
  /* ************ Reading field: noPN ************* */
  if (P_ERR==P_io_checkpoint (pads,1)) 
    {
      PDCI_report_err (pads,P_LEV_FATAL,0,P_CHKPOINT_ERR,0);
    }
  modrep->tag = noPN;
  if (P_ERR==no_pn_vbar_read_internal (pads,&(modem->noPN),&(moded->noPN),&((modrep->val).noPN))) 
    {
      if (P_ERR==P_io_restore (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_RESTORE_ERR,0);
        }
    }
  else
    {
      if (P_ERR==P_io_commit (pads)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_COMMIT_ERR,0);
        }
      return P_OK;
    }
  /* ********* We didn't match any branch ********* */
  (moded->nerr)++;
  moded->errCode = P_UNION_MATCH_ERR;
  P_io_getLoc (pads,&(moded->loc),0);
  PDCI_report_err (pads,P_LEV_INFO,&(moded->loc),moded->errCode,"Did not match any branch of union dib_pn_vbar.");
  moded->panic = 1;
  return P_ERR;
}
Perror_t dib_pn_vbar_read (P_t *pads,dib_pn_vbar_em *em,dib_pn_vbar_ed *ed,dib_pn_vbar *rep)
{
  dib_pn_vbar tmprep={(enum dib_pn_vbar_tag_e) 0,{{0}}};
  dib_pn_vbar *modrep=rep;
  dib_pn_vbar_em tmpem={{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}};
  dib_pn_vbar_em *modem=em;
  dib_pn_vbar_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}};
  dib_pn_vbar_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"dib_pn_vbar_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"dib_pn_vbar_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"dib_pn_vbar_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"dib_pn_vbar_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return dib_pn_vbar_read_internal (pads,modem,moded,modrep);
}
Perror_t dib_pn_vbar_acc_init (P_t *pads,dib_pn_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==auint64_vbar_acc_init (pads,&(acc->yesPN))) 
    {
      nerr++;
    }
  if (P_ERR==no_pn_vbar_acc_init (pads,&(acc->noPN))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t dib_pn_vbar_acc_reset (P_t *pads,dib_pn_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==auint64_vbar_acc_reset (pads,&(acc->yesPN))) 
    {
      nerr++;
    }
  if (P_ERR==no_pn_vbar_acc_reset (pads,&(acc->noPN))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t dib_pn_vbar_acc_cleanup (P_t *pads,dib_pn_vbar_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->tag))) 
    {
      nerr++;
    }
  if (P_ERR==auint64_vbar_acc_cleanup (pads,&(acc->yesPN))) 
    {
      nerr++;
    }
  if (P_ERR==no_pn_vbar_acc_cleanup (pads,&(acc->noPN))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t dib_pn_vbar_acc_add (P_t *pads,dib_pn_vbar_acc *acc,dib_pn_vbar_ed *ed,dib_pn_vbar *rep)
{
  int nerr=0;
  Pbase_ed ted;
  ted.errCode = P_NO_ERR;
  if (P_ERR==Pint32_acc_add (pads,&(acc->tag),&ted,(Pint32 *) (&(rep->tag)))) 
    {
      nerr++;
    }
  switch (rep->tag)
    {
      
    case 1: 
      {
        if (P_ERR==auint64_vbar_acc_add (pads,&(acc->yesPN),&(ed->yesPN),&((rep->val).yesPN))) 
          {
            nerr++;
          }
        break;
      }
      
    case 2: 
      {
        if (P_ERR==no_pn_vbar_acc_add (pads,&(acc->noPN),&(ed->noPN),&((rep->val).noPN))) 
          {
            nerr++;
          }
        break;
      }
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t dib_pn_vbar_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,dib_pn_vbar_acc *acc)
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
      what = "union dib_pn_vbar";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  if (P_ERR==Pint32_acc_report_map_internal (pads,outstr,"Union tag","tag",-1,(Pint32_map_fn) dib_pn_vbar_tag2str,&(acc->tag))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (outstr,"\n[Describing each tag arm of %s]\n",prefix);
  sfprintf (tmpstr,"%s.yesPN",prefix);
  if (P_ERR==auint64_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->yesPN))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.noPN",prefix);
  if (P_ERR==no_pn_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->noPN))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t dib_pn_vbar_acc_report (P_t *pads,char const *prefix,char const *what,int nst,dib_pn_vbar_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = dib_pn_vbar_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t event_read_internal (P_t *pads,event_em *modem,event_ed *moded,event *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  /* *********** Reading field: state. ************ */
  /* XXX_OPT: NOT NEEDED */
#if 0
  if (moded->panic) 
    {
      (moded->state).panic = 1;
      (moded->state).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->state).loc),0);
      (moded->nerr)+=1;
    }
  else
#endif
    {
      if (P_ERR==Pa_string_read_internal (pads,&(modem->state),124,&(moded->state),&(modrep->state))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->state).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->state).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  {
    /* ********* Reading delimiter field: | ********* */
    {
      Pbase_ed ted;
      if (moded->panic) 
        {
          size_t n;
          if (P_ERR!=Pa_char_lit_scan (pads,124,124,1,0,&n)) 
            {
              moded->panic = 0;
            }
        }
      else
        {
          Pbase_em tem=P_Check;
          if (P_ERR==Pchar_lit_read_internal (pads,&tem,&ted,124)) 
            {
              if (P_spec_level (pads)) 
                {
                  return P_ERR;
                }
              PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_MISSING_LITERAL,"Missing separator: %s.","|");
              if (0==(moded->nerr)) 
                {
                  moded->errCode = P_MISSING_LITERAL;
                  moded->loc = (ted.loc);
                }
              (moded->nerr)+=1;
              moded->panic = 1;
            }
        }
    }
  }
  /* *********** Reading field: tstamp. *********** */
  if (moded->panic) 
    {
      (moded->tstamp).panic = 1;
      (moded->tstamp).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->tstamp).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==Pa_uint32_read_internal (pads,&(modem->tstamp),&(moded->tstamp),&(modrep->tstamp))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->tstamp).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->tstamp).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  {
    /* ********* Reading delimiter field: | ********* */
    {
      Pbase_ed ted;
      if (moded->panic) 
        {
          size_t n;
          if (P_ERR!=Pa_char_lit_scan (pads,124,124,1,0,&n)) 
            {
              moded->panic = 0;
            }
        }
      else
        {
          Pbase_em tem=P_Check;
          if (P_ERR==Pchar_lit_read_internal (pads,&tem,&ted,124)) 
            {
              if (P_spec_level (pads)) 
                {
                  return P_ERR;
                }
              PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_MISSING_LITERAL,"Missing separator: %s.","|");
              if (0==(moded->nerr)) 
                {
                  moded->errCode = P_MISSING_LITERAL;
                  moded->loc = (ted.loc);
                }
              (moded->nerr)+=1;
              moded->panic = 1;
            }
        }
    }
  }
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t event_read (P_t *pads,event_em *em,event_ed *ed,event *rep)
{
  event tmprep={{0,0,0},0};
  event *modrep=rep;
  event_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),(enum Pbase_em_e) 0,(enum Pbase_em_e) 0};
  event_em *modem=em;
  event_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}};
  event_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"event_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"event_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"event_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"event_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return event_read_internal (pads,modem,moded,modrep);
}
Perror_t event_acc_init (P_t *pads,event_acc *acc)
{
  int nerr=0;
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
Perror_t event_acc_reset (P_t *pads,event_acc *acc)
{
  int nerr=0;
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
Perror_t event_acc_cleanup (P_t *pads,event_acc *acc)
{
  int nerr=0;
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
Perror_t event_acc_add (P_t *pads,event_acc *acc,event_ed *ed,event *rep)
{
  int nerr=0;
  if (P_ERR==Pstring_acc_add (pads,&(acc->state),&(ed->state),&(rep->state))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_add (pads,&(acc->tstamp),&(ed->tstamp),&(rep->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t event_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,event_acc *acc)
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
      what = "struct event";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.state",prefix);
  if (P_ERR==Pstring_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->state))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.tstamp",prefix);
  if (P_ERR==Puint32_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->tstamp))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t event_acc_report (P_t *pads,char const *prefix,char const *what,int nst,event_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = event_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t event_init (P_t *pads,event *rep)
{
  if ((!pads)||(!rep)) 
    return P_ERR;
  Pstring_init (pads,&(rep->state));
  return P_OK;
}
Perror_t event_ed_init (P_t *pads,event_ed *ed)
{
  if ((!pads)||(!ed)) 
    return P_ERR;
  Pstring_ed_init (pads,&(ed->state));
  return P_OK;
}
Perror_t event_cleanup (P_t *pads,event *rep)
{
  if ((!pads)||(!rep)) 
    return P_ERR;
  Pstring_cleanup (pads,&(rep->state));
  return P_OK;
}
Perror_t event_ed_cleanup (P_t *pads,event_ed *ed)
{
  if ((!pads)||(!ed)) 
    return P_ERR;
  Pstring_ed_cleanup (pads,&(ed->state));
  return P_OK;
}
Perror_t out_sum_header_read_internal (P_t *pads,out_sum_header_em *modem,out_sum_header_ed *moded,out_sum_header *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  {
    /* ******* Reading delimiter field: "0|" ******** */
    {
      Pbase_ed ted;
      Pstring strlit={"0|",0,0,0};
      strlit.len = 2;
  /* XXX_OPT: NOT NEEDED */
#if 0
      if (moded->panic) 
        {
          size_t n;
          if (P_ERR!=Pstr_lit_scan (pads,&strlit,&strlit,1,0,&n)) 
            {
              moded->panic = 0;
            }
        }
      else
#endif
        {
          Pbase_em tem=P_Check;
          if (P_ERR==Pstr_lit_read_internal (pads,&tem,&ted,&strlit)) 
            {
              if (P_spec_level (pads)) 
                {
                  return P_ERR;
                }
              PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_MISSING_LITERAL,"Missing separator: %s.","\"0|\"");
              if (0==(moded->nerr)) 
                {
                  moded->errCode = P_MISSING_LITERAL;
                  moded->loc = (ted.loc);
                }
              (moded->nerr)+=1;
              moded->panic = 1;
            }
        }
    }
  }
  /* *********** Reading field: tstamp. *********** */
  if (moded->panic) 
    {
      (moded->tstamp).panic = 1;
      (moded->tstamp).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->tstamp).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==Pa_uint32_read_internal (pads,&(modem->tstamp),&(moded->tstamp),&(modrep->tstamp))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->tstamp).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->tstamp).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ******** Reading delimiter field: EOR ******** */
  {
    Pbase_ed ted;
    size_t n;
    P_io_getLocB (pads,&(ted.loc),0);
    if (P_OK==P_io_next_rec (pads,&n)) 
      {
        if (n>0) 
          {
            if (P_spec_level (pads)) 
              {
                return P_ERR;
              }
            P_io_getLocE (pads,&(ted.loc),0);
            if (!(moded->panic)) 
              {
                PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_EXTRA_BEFORE_EOR,0);
                if (0==(moded->nerr)) 
                  {
                    moded->errCode = P_EXTRA_BEFORE_EOR;
                    moded->loc = (ted.loc);
                  }
                (moded->nerr)+=1;
              }
            else
              {
                P_io_getLoc (pads,&(ted.loc),0);
                PDCI_report_err (pads,P_LEV_ERR,&(ted.loc),P_NO_ERR,"Resynching at EOR");
              }
          }
        moded->panic = 0;
      }
    else
      {
        if (P_spec_level (pads)) 
          {
            return P_ERR;
          }
        moded->panic = 0;
        P_io_getLocE (pads,&(ted.loc),0);
        PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_AT_EOR,"Found EOF when searching for EOR");
      }
  }
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t out_sum_header_read (P_t *pads,out_sum_header_em *em,out_sum_header_ed *ed,out_sum_header *rep)
{
  out_sum_header tmprep={0};
  out_sum_header *modrep=rep;
  out_sum_header_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),(enum Pbase_em_e) 0};
  out_sum_header_em *modem=em;
  out_sum_header_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}};
  out_sum_header_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"out_sum_header_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"out_sum_header_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"out_sum_header_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"out_sum_header_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return out_sum_header_read_internal (pads,modem,moded,modrep);
}
Perror_t out_sum_header_acc_init (P_t *pads,out_sum_header_acc *acc)
{
  int nerr=0;
  if (P_ERR==Puint32_acc_init (pads,&(acc->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_header_acc_reset (P_t *pads,out_sum_header_acc *acc)
{
  int nerr=0;
  if (P_ERR==Puint32_acc_reset (pads,&(acc->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_header_acc_cleanup (P_t *pads,out_sum_header_acc *acc)
{
  int nerr=0;
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_header_acc_add (P_t *pads,out_sum_header_acc *acc,out_sum_header_ed *ed,out_sum_header *rep)
{
  int nerr=0;
  if (P_ERR==Puint32_acc_add (pads,&(acc->tstamp),&(ed->tstamp),&(rep->tstamp))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_header_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,out_sum_header_acc *acc)
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
      what = "struct out_sum_header";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.tstamp",prefix);
  if (P_ERR==Puint32_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->tstamp))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t out_sum_header_acc_report (P_t *pads,char const *prefix,char const *what,int nst,out_sum_header_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = out_sum_header_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t eventSeq_read_internal (P_t *pads,eventSeq_em *modem,int size,eventSeq_ed *moded,eventSeq *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  {
    /*     Pbase_em tem=P_Check; */
    /*    Pbase_ed ted; */
    int reachedLimit=0;
    modrep->length = 0;
    if (((modem->array)<=P_Check)&&(size<0)) 
      {
        if (P_spec_level (pads)) 
          return P_ERR;
        if (!(moded->nerr)) 
          {
            (moded->nerr)++;
            moded->errCode = P_ARRAY_MAX_NEGATIVE;
            P_io_getLoc (pads,&(moded->loc),0);
            PDCI_report_err (pads,P_LEV_INFO,&(moded->loc),moded->errCode,"Maximum value for the size of array eventSeq(%d) is negative.",size);
          }
        else
          {
            (moded->nerr)++;
          }
        moded->panic = 1;
      }
    if (0==(modrep->_internal)) 
      {
        modrep->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(modrep->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"");
          }
      }
    if (0==(moded->_internal)) 
      {
        moded->_internal = RMM_new_rbuf (P_rmm_zero (pads));
        if (0==(moded->_internal)) 
          {
            PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"");
          }
      }
    /* 
 Reading input until we reach a termination condition 
 */
    if ((!(moded->panic))&&(!P_io_at_eof (pads))) 
      {
        if ((modrep->length)>=size) 
          {
            reachedLimit = 1;
          }
        else
          {
            while (1)
              {
                /* ******** Ready to read next element. ********* */
                (modrep->length)++;
                reachedLimit = ((modrep->length)>=size);
                if (0!=RBuf_reserve (modrep->_internal,(void **) (&(modrep->eventSeq)),sizeof(event),modrep->length,size)) 
                  {
                    PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,0);
                  }
                if (0!=RBuf_reserve (moded->_internal,(void **) (&(moded->eventSeq)),sizeof(event_ed),modrep->length,size)) 
                  {
                    PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,0);
                  }
                if (P_ERR==event_read_internal (pads,&(modem->element),&(moded->eventSeq)[(modrep->length)-1],&(modrep->eventSeq)[(modrep->length)-1])) 
                  {
                    if (P_spec_level (pads)) 
                      return P_ERR;
                    if ((modem->array)<=P_Check) 
                      {
                        if (!(moded->nerr)) 
                          {
                            (moded->nerr)++;
                            moded->errCode = P_ARRAY_ELEM_ERR;
                            P_io_getLoc (pads,&(moded->loc),0);
                            /* *** Index of first element with an error. **** */
                            moded->firstError = ((modrep->length)-1);
                          }
                        (moded->neerr)++;
                      }
                  }
                if ((moded->eventSeq)[(modrep->length)-1].panic) 
                  {
                    {
                      /* *********** No recovery possible. ************ */
                      moded->panic = 1;
                      break;
                    }
                  }
                /* ****** Have we finished reading array? ******* */
                if (P_io_at_eof (pads)||reachedLimit) 
                  {
                    break;
                  }
              }
          }
      }
    moded->length = (modrep->length);
    return ((moded->nerr)==0) ? P_OK : P_ERR;
  }
}
Perror_t eventSeq_read (P_t *pads,eventSeq_em *em,int size,eventSeq_ed *ed,eventSeq *rep)
{
  eventSeq tmprep={0,0,0};
  eventSeq *modrep=rep;
  eventSeq_em tmpem={{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},(enum Pbase_em_e) 0};
  eventSeq_em *modem=em;
  eventSeq_ed tmped={0,(enum PerrCode_t_e) 0,0,{{0,0,0},{0,0,0}},0,0,0,0,0};
  eventSeq_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"eventSeq_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"eventSeq_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"eventSeq_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"eventSeq_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return eventSeq_read_internal (pads,modem,size,moded,modrep);
}
Perror_t eventSeq_acc_init (P_t *pads,eventSeq_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->length))) 
    {
      nerr++;
    }
  if (P_ERR==event_acc_init (pads,&(acc->array))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        if (P_ERR==event_acc_init (pads,&(acc->arrayDetail)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t eventSeq_acc_reset (P_t *pads,eventSeq_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->length))) 
    {
      nerr++;
    }
  if (P_ERR==event_acc_reset (pads,&(acc->array))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        if (P_ERR==event_acc_reset (pads,&(acc->arrayDetail)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t eventSeq_acc_cleanup (P_t *pads,eventSeq_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->length))) 
    {
      nerr++;
    }
  if (P_ERR==event_acc_cleanup (pads,&(acc->array))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        if (P_ERR==event_acc_cleanup (pads,&(acc->arrayDetail)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t eventSeq_acc_add (P_t *pads,eventSeq_acc *acc,eventSeq_ed *ed,eventSeq *rep)
{
  int nerr=0;
  Pbase_ed ted;
  ted.errCode = P_NO_ERR;
  if (P_ERR==Pint32_acc_add (pads,&(acc->length),&ted,&(rep->length))) 
    {
      nerr++;
    }
  {
    int i;
    for (i = 0; i<(rep->length); i++)
      {
        if (i<10) 
          {
            if (P_ERR==event_acc_add (pads,&(acc->arrayDetail)[i],&(ed->eventSeq)[i],&(rep->eventSeq)[i])) 
              {
                nerr++;
              }
          }
        if (P_ERR==event_acc_add (pads,&(acc->array),&(ed->eventSeq)[i],&(rep->eventSeq)[i])) 
          {
            nerr++;
          }
      }
  }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t eventSeq_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,eventSeq_acc *acc)
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
      what = "array eventSeq of event";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  if (P_ERR==Pint32_acc_report_internal (pads,outstr,"Array lengths","lengths",-1,&(acc->length))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.allArrayElts",prefix);
  if (P_ERR==event_acc_report_internal (pads,outstr,sfstruse (tmpstr),"all array elements",nst,&(acc->array))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  {
    int i;
    for (i = 0; i<10; i++)
      {
        sfprintf (tmpstr,"%s.arrayDetail[%d]",prefix,i);
        if (P_ERR==event_acc_report_internal (pads,outstr,sfstruse (tmpstr),"array element",nst,&(acc->arrayDetail)[i])) 
          {
            sfstrclose (tmpstr);
            return P_ERR;
          }
      }
  }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t eventSeq_acc_report (P_t *pads,char const *prefix,char const *what,int nst,eventSeq_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = eventSeq_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t eventSeq_init (P_t *pads,eventSeq *rep)
{
  if ((!pads)||(!rep)) 
    return P_ERR;
  rep->length = 0;
  rep->eventSeq = 0;
  rep->_internal = 0;
  return P_OK;
}
Perror_t eventSeq_ed_init (P_t *pads,eventSeq_ed *ed)
{
  if ((!pads)||(!ed)) 
    return P_ERR;
  ed->length = 0;
  ed->eventSeq = 0;
  ed->_internal = 0;
  return P_OK;
}
Perror_t eventSeq_cleanup (P_t *pads,eventSeq *rep)
{
  if ((!pads)||(!rep)) 
    return P_ERR;
  rep->length = 0;
  rep->eventSeq = 0;
  if (rep->_internal) 
    {
      if (0!=RMM_free_rbuf (rep->_internal)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"Couldn\'t free growable buffer");
        }
    }
  return P_OK;
}
Perror_t eventSeq_ed_cleanup (P_t *pads,eventSeq_ed *ed)
{
  if ((!pads)||(!ed)) 
    return P_ERR;
  ed->length = 0;
  ed->eventSeq = 0;
  if (ed->_internal) 
    {
      if (0!=RMM_free_rbuf (ed->_internal)) 
        {
          PDCI_report_err (pads,P_LEV_FATAL,0,P_ALLOC_ERR,"Couldn\'t free growable buffer");
        }
    }
  return P_OK;
}
int getLen (int numBars)
{
  return (numBars-4)/2;
}
Perror_t out_sum_fixed1_read_internal (P_t *pads,out_sum_fixed1_em *modem,out_sum_fixed1_ed *moded,out_sum_fixed1 *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  /* ********* Reading field: order_num. ********** */
  /* XXX_OPT: NOT NEEDED */
#if 0
  if (moded->panic) 
    {
      (moded->order_num).panic = 1;
      (moded->order_num).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->order_num).loc),0);
      (moded->nerr)+=1;
    }
  else
#endif
    {
      if (P_ERR==auint32_vbar_read_internal (pads,&(modem->order_num),&(moded->order_num),&(modrep->order_num))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->order_num).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->order_num).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ********* Reading field: order_item. ********* */
  if (moded->panic) 
    {
      (moded->order_item).panic = 1;
      (moded->order_item).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->order_item).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==auint32_vbar_read_internal (pads,&(modem->order_item),&(moded->order_item),&(modrep->order_item))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->order_item).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->order_item).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ********** Reading field: servicen. ********** */
  if (moded->panic) 
    {
      (moded->servicen).panic = 1;
      (moded->servicen).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->servicen).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==dib_pn_vbar_read_internal (pads,&(modem->servicen),&(moded->servicen),&(modrep->servicen))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->servicen).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->servicen).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ********* Reading field: billing_tn. ********* */
  if (moded->panic) 
    {
      (moded->billing_tn).panic = 1;
      (moded->billing_tn).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->billing_tn).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==dib_pn_vbar_read_internal (pads,&(modem->billing_tn),&(moded->billing_tn),&(modrep->billing_tn))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->billing_tn).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->billing_tn).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ********** Reading field: zip_code. ********** */
  if (moded->panic) 
    {
      (moded->zip_code).panic = 1;
      (moded->zip_code).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->zip_code).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==auint32_vbar_read_internal (pads,&(modem->zip_code),&(moded->zip_code),&(modrep->zip_code))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->zip_code).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->zip_code).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ******* Reading field: nlp_service_tn. ******* */
  if (moded->panic) 
    {
      (moded->nlp_service_tn).panic = 1;
      (moded->nlp_service_tn).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->nlp_service_tn).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==dib_pn_vbar_read_internal (pads,&(modem->nlp_service_tn),&(moded->nlp_service_tn),&(modrep->nlp_service_tn))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->nlp_service_tn).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->nlp_service_tn).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ******* Reading field: nlp_billing_tn. ******* */
  if (moded->panic) 
    {
      (moded->nlp_billing_tn).panic = 1;
      (moded->nlp_billing_tn).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->nlp_billing_tn).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==dib_pn_vbar_read_internal (pads,&(modem->nlp_billing_tn),&(moded->nlp_billing_tn),&(modrep->nlp_billing_tn))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->nlp_billing_tn).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->nlp_billing_tn).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed1_read (P_t *pads,out_sum_fixed1_em *em,out_sum_fixed1_ed *ed,out_sum_fixed1 *rep)
{
  out_sum_fixed1 tmprep={{0},{0},{(enum dib_pn_vbar_tag_e) 0,{{0}}},{(enum dib_pn_vbar_tag_e) 0,{{0}}},{0},{(enum dib_pn_vbar_tag_e) 0,{{0}}},{(enum dib_pn_vbar_tag_e) 0,{{0}}}};
  out_sum_fixed1 *modrep=rep;
  out_sum_fixed1_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}}};
  out_sum_fixed1_em *modem=em;
  out_sum_fixed1_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}}};
  out_sum_fixed1_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"out_sum_fixed1_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"out_sum_fixed1_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"out_sum_fixed1_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"out_sum_fixed1_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return out_sum_fixed1_read_internal (pads,modem,moded,modrep);
}
Perror_t out_sum_fixed1_acc_init (P_t *pads,out_sum_fixed1_acc *acc)
{
  int nerr=0;
  if (P_ERR==auint32_vbar_acc_init (pads,&(acc->order_num))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_init (pads,&(acc->order_item))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_init (pads,&(acc->servicen))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_init (pads,&(acc->billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_init (pads,&(acc->zip_code))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_init (pads,&(acc->nlp_service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_init (pads,&(acc->nlp_billing_tn))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed1_acc_reset (P_t *pads,out_sum_fixed1_acc *acc)
{
  int nerr=0;
  if (P_ERR==auint32_vbar_acc_reset (pads,&(acc->order_num))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_reset (pads,&(acc->order_item))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_reset (pads,&(acc->servicen))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_reset (pads,&(acc->billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_reset (pads,&(acc->zip_code))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_reset (pads,&(acc->nlp_service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_reset (pads,&(acc->nlp_billing_tn))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed1_acc_cleanup (P_t *pads,out_sum_fixed1_acc *acc)
{
  int nerr=0;
  if (P_ERR==auint32_vbar_acc_cleanup (pads,&(acc->order_num))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_cleanup (pads,&(acc->order_item))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_cleanup (pads,&(acc->servicen))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_cleanup (pads,&(acc->billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_cleanup (pads,&(acc->zip_code))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_cleanup (pads,&(acc->nlp_service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_cleanup (pads,&(acc->nlp_billing_tn))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed1_acc_add (P_t *pads,out_sum_fixed1_acc *acc,out_sum_fixed1_ed *ed,out_sum_fixed1 *rep)
{
  int nerr=0;
  if (P_ERR==auint32_vbar_acc_add (pads,&(acc->order_num),&(ed->order_num),&(rep->order_num))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_add (pads,&(acc->order_item),&(ed->order_item),&(rep->order_item))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_add (pads,&(acc->servicen),&(ed->servicen),&(rep->servicen))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_add (pads,&(acc->billing_tn),&(ed->billing_tn),&(rep->billing_tn))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_add (pads,&(acc->zip_code),&(ed->zip_code),&(rep->zip_code))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_add (pads,&(acc->nlp_service_tn),&(ed->nlp_service_tn),&(rep->nlp_service_tn))) 
    {
      nerr++;
    }
  if (P_ERR==dib_pn_vbar_acc_add (pads,&(acc->nlp_billing_tn),&(ed->nlp_billing_tn),&(rep->nlp_billing_tn))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed1_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,out_sum_fixed1_acc *acc)
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
      what = "struct out_sum_fixed1";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.order_num",prefix);
  if (P_ERR==auint32_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->order_num))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.order_item",prefix);
  if (P_ERR==auint32_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->order_item))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.servicen",prefix);
  if (P_ERR==dib_pn_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->servicen))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.billing_tn",prefix);
  if (P_ERR==dib_pn_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->billing_tn))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.zip_code",prefix);
  if (P_ERR==auint32_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->zip_code))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.nlp_service_tn",prefix);
  if (P_ERR==dib_pn_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->nlp_service_tn))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.nlp_billing_tn",prefix);
  if (P_ERR==dib_pn_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->nlp_billing_tn))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t out_sum_fixed1_acc_report (P_t *pads,char const *prefix,char const *what,int nst,out_sum_fixed1_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = out_sum_fixed1_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t out_sum_fixed2_read_internal (P_t *pads,out_sum_fixed2_em *modem,out_sum_fixed2_ed *moded,out_sum_fixed2 *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  /* ************ Reading field: siid. ************ */
  /* XXX_OPT: NOT NEEDED */
#if 0
  if (moded->panic) 
    {
      (moded->siid).panic = 1;
      (moded->siid).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->siid).loc),0);
      (moded->nerr)+=1;
    }
  else
#endif
    {
      if (P_ERR==opt_auint32_vbar_read_internal (pads,&(modem->siid),&(moded->siid),&(modrep->siid))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->siid).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->siid).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ********* Reading field: create_id. ********** */
  if (moded->panic) 
    {
      (moded->create_id).panic = 1;
      (moded->create_id).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->create_id).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==opt_auint32_vbar_read_internal (pads,&(modem->create_id),&(moded->create_id),&(modrep->create_id))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->create_id).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->create_id).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* *********** Reading field: rampII. *********** */
  if (moded->panic) 
    {
      (moded->rampII).panic = 1;
      (moded->rampII).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->rampII).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==opt_auint64_vbar_read_internal (pads,&(modem->rampII),&(moded->rampII),&(modrep->rampII))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->rampII).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->rampII).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ********* Reading field: order_type. ********* */
  if (moded->panic) 
    {
      (moded->order_type).panic = 1;
      (moded->order_type).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->order_type).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==auint32_vbar_read_internal (pads,&(modem->order_type),&(moded->order_type),&(modrep->order_type))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->order_type).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->order_type).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ******** Reading field: parent_order. ******** */
  if (moded->panic) 
    {
      (moded->parent_order).panic = 1;
      (moded->parent_order).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->parent_order).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==Pa_uint32_read_internal (pads,&(modem->parent_order),&(moded->parent_order),&(modrep->parent_order))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->parent_order).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->parent_order).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed2_read (P_t *pads,out_sum_fixed2_em *em,out_sum_fixed2_ed *ed,out_sum_fixed2 *rep)
{
  out_sum_fixed2 tmprep={{(enum opt_auint32_vbar_tag_e) 0,{{0}}},{(enum opt_auint32_vbar_tag_e) 0,{{0}}},{(enum opt_auint64_vbar_tag_e) 0,{{0}}},{0},0};
  out_sum_fixed2 *modrep=rep;
  out_sum_fixed2_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},(enum Pbase_em_e) 0};
  out_sum_fixed2_em *modem=em;
  out_sum_fixed2_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}};
  out_sum_fixed2_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"out_sum_fixed2_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"out_sum_fixed2_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"out_sum_fixed2_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"out_sum_fixed2_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return out_sum_fixed2_read_internal (pads,modem,moded,modrep);
}
Perror_t out_sum_fixed2_acc_init (P_t *pads,out_sum_fixed2_acc *acc)
{
  int nerr=0;
  if (P_ERR==opt_auint32_vbar_acc_init (pads,&(acc->siid))) 
    {
      nerr++;
    }
  if (P_ERR==opt_auint32_vbar_acc_init (pads,&(acc->create_id))) 
    {
      nerr++;
    }
  if (P_ERR==opt_auint64_vbar_acc_init (pads,&(acc->rampII))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_init (pads,&(acc->order_type))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_init (pads,&(acc->parent_order))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed2_acc_reset (P_t *pads,out_sum_fixed2_acc *acc)
{
  int nerr=0;
  if (P_ERR==opt_auint32_vbar_acc_reset (pads,&(acc->siid))) 
    {
      nerr++;
    }
  if (P_ERR==opt_auint32_vbar_acc_reset (pads,&(acc->create_id))) 
    {
      nerr++;
    }
  if (P_ERR==opt_auint64_vbar_acc_reset (pads,&(acc->rampII))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_reset (pads,&(acc->order_type))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_reset (pads,&(acc->parent_order))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed2_acc_cleanup (P_t *pads,out_sum_fixed2_acc *acc)
{
  int nerr=0;
  if (P_ERR==opt_auint32_vbar_acc_cleanup (pads,&(acc->siid))) 
    {
      nerr++;
    }
  if (P_ERR==opt_auint32_vbar_acc_cleanup (pads,&(acc->create_id))) 
    {
      nerr++;
    }
  if (P_ERR==opt_auint64_vbar_acc_cleanup (pads,&(acc->rampII))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_cleanup (pads,&(acc->order_type))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_cleanup (pads,&(acc->parent_order))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed2_acc_add (P_t *pads,out_sum_fixed2_acc *acc,out_sum_fixed2_ed *ed,out_sum_fixed2 *rep)
{
  int nerr=0;
  if (P_ERR==opt_auint32_vbar_acc_add (pads,&(acc->siid),&(ed->siid),&(rep->siid))) 
    {
      nerr++;
    }
  if (P_ERR==opt_auint32_vbar_acc_add (pads,&(acc->create_id),&(ed->create_id),&(rep->create_id))) 
    {
      nerr++;
    }
  if (P_ERR==opt_auint64_vbar_acc_add (pads,&(acc->rampII),&(ed->rampII),&(rep->rampII))) 
    {
      nerr++;
    }
  if (P_ERR==auint32_vbar_acc_add (pads,&(acc->order_type),&(ed->order_type),&(rep->order_type))) 
    {
      nerr++;
    }
  if (P_ERR==Puint32_acc_add (pads,&(acc->parent_order),&(ed->parent_order),&(rep->parent_order))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_fixed2_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,out_sum_fixed2_acc *acc)
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
      what = "struct out_sum_fixed2";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.siid",prefix);
  if (P_ERR==opt_auint32_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->siid))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.create_id",prefix);
  if (P_ERR==opt_auint32_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->create_id))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.rampII",prefix);
  if (P_ERR==opt_auint64_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->rampII))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.order_type",prefix);
  if (P_ERR==auint32_vbar_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->order_type))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.parent_order",prefix);
  if (P_ERR==Puint32_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->parent_order))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t out_sum_fixed2_acc_report (P_t *pads,char const *prefix,char const *what,int nst,out_sum_fixed2_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = out_sum_fixed2_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t do_ev_count_read_internal (P_t *pads,do_ev_count_em *modem,do_ev_count_ed *moded,do_ev_count *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  {
    Pint32 bars;
  /* XXX_OPT: NOT NEEDED */
#if 0
    /* ************ Reading field: bars. ************ */
    if (moded->panic) 
      {
        (moded->bars).panic = 1;
        (moded->bars).errCode = P_PANIC_SKIPPED;
        P_io_getLoc (pads,&((moded->bars).loc),0);
        (moded->nerr)+=1;
      }
    else
#endif
      {
        if (P_ERR==PcountX_internal(pads,&(modem->bars),124,1,&(moded->bars),&bars)) 
          {
            if (P_spec_level (pads)) 
              {
                return P_ERR;
              }
            if ((moded->bars).panic) 
              {
                moded->panic = 1;
              }
            if (0==(moded->nerr)) 
              {
                moded->errCode = P_STRUCT_FIELD_ERR;
                moded->loc = ((moded->bars).loc);
              }
            (moded->nerr)+=1;
          }
        else
          {
          }
      }
    /* ********** Reading field: ev_count. ********** */
  /* XXX_OPT: DO NOT DO DUMMY STUFF */
    modrep->ev_count = getLen (bars); /* XXX_OPT JUST DO THIS */
#if 0
    if (moded->panic) 
      {
        (moded->ev_count).panic = 1;
        (moded->ev_count).errCode = P_PANIC_SKIPPED;
        P_io_getLoc (pads,&((moded->ev_count).loc),0);
        (moded->nerr)+=1;
      }
    else
      {
        if (P_ERR==Pdummy_read_internal (pads,&(modem->ev_count),getLen (bars),&(moded->ev_count),&(modrep->ev_count))) 
          {
            if (P_spec_level (pads)) 
              {
                return P_ERR;
              }
            if ((moded->ev_count).panic) 
              {
                moded->panic = 1;
              }
            if (0==(moded->nerr)) 
              {
                moded->errCode = P_STRUCT_FIELD_ERR;
                moded->loc = ((moded->ev_count).loc);
              }
            (moded->nerr)+=1;
          }
        else
          {
          }
      }
#endif
  }
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t do_ev_count_read (P_t *pads,do_ev_count_em *em,do_ev_count_ed *ed,do_ev_count *rep)
{
  do_ev_count tmprep={0};
  do_ev_count *modrep=rep;
  do_ev_count_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),(enum Pbase_em_e) 0,(enum Pbase_em_e) 0};
  do_ev_count_em *modem=em;
  do_ev_count_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}};
  do_ev_count_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"do_ev_count_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"do_ev_count_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"do_ev_count_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"do_ev_count_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return do_ev_count_read_internal (pads,modem,moded,modrep);
}
Perror_t do_ev_count_acc_init (P_t *pads,do_ev_count_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_init (pads,&(acc->ev_count))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t do_ev_count_acc_reset (P_t *pads,do_ev_count_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_reset (pads,&(acc->ev_count))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t do_ev_count_acc_cleanup (P_t *pads,do_ev_count_acc *acc)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_cleanup (pads,&(acc->ev_count))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t do_ev_count_acc_add (P_t *pads,do_ev_count_acc *acc,do_ev_count_ed *ed,do_ev_count *rep)
{
  int nerr=0;
  if (P_ERR==Pint32_acc_add (pads,&(acc->ev_count),&(ed->ev_count),&(rep->ev_count))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t do_ev_count_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,do_ev_count_acc *acc)
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
      what = "struct do_ev_count";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.ev_count",prefix);
  if (P_ERR==Pint32_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->ev_count))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t do_ev_count_acc_report (P_t *pads,char const *prefix,char const *what,int nst,do_ev_count_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = do_ev_count_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t out_sum_data_line_read_internal (P_t *pads,out_sum_data_line_em *modem,out_sum_data_line_ed *moded,out_sum_data_line *modrep)
{
  moded->nerr = 0;
  moded->panic = 0;
  /* ************* Reading field: f1. ************* */
  /* XXX_OPT: NOT NEEDED */
#if 0
  if (moded->panic) 
    {
      (moded->f1).panic = 1;
      (moded->f1).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->f1).loc),0);
      (moded->nerr)+=1;
    }
  else
#endif
    {
      if (P_ERR==out_sum_fixed1_read_internal (pads,&(modem->f1),&(moded->f1),&(modrep->f1))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->f1).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->f1).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ************* Reading field: c. ************** */
  if (moded->panic) 
    {
      (moded->c).panic = 1;
      (moded->c).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->c).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==do_ev_count_read_internal (pads,&(modem->c),&(moded->c),&(modrep->c))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->c).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->c).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* *********** Reading field: events. *********** */
  if (moded->panic) 
    {
      (moded->events).panic = 1;
      (moded->events).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->events).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==eventSeq_read_internal (pads,&(modem->events),(modrep->c).ev_count,&(moded->events),&(modrep->events))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->events).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->events).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ************* Reading field: f2. ************* */
  if (moded->panic) 
    {
      (moded->f2).panic = 1;
      (moded->f2).errCode = P_PANIC_SKIPPED;
      P_io_getLoc (pads,&((moded->f2).loc),0);
      (moded->nerr)+=1;
    }
  else
    {
      if (P_ERR==out_sum_fixed2_read_internal (pads,&(modem->f2),&(moded->f2),&(modrep->f2))) 
        {
          if (P_spec_level (pads)) 
            {
              return P_ERR;
            }
          if ((moded->f2).panic) 
            {
              moded->panic = 1;
            }
          if (0==(moded->nerr)) 
            {
              moded->errCode = P_STRUCT_FIELD_ERR;
              moded->loc = ((moded->f2).loc);
            }
          (moded->nerr)+=1;
        }
      else
        {
        }
    }
  /* ******** Reading delimiter field: EOR ******** */
  {
    Pbase_ed ted;
    size_t n;
    P_io_getLocB (pads,&(ted.loc),0);
    if (P_OK==P_io_next_rec (pads,&n)) 
      {
        if (n>0) 
          {
            if (P_spec_level (pads)) 
              {
                return P_ERR;
              }
            P_io_getLocE (pads,&(ted.loc),0);
            if (!(moded->panic)) 
              {
                PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_EXTRA_BEFORE_EOR,0);
                if (0==(moded->nerr)) 
                  {
                    moded->errCode = P_EXTRA_BEFORE_EOR;
                    moded->loc = (ted.loc);
                  }
                (moded->nerr)+=1;
              }
            else
              {
                P_io_getLoc (pads,&(ted.loc),0);
                PDCI_report_err (pads,P_LEV_ERR,&(ted.loc),P_NO_ERR,"Resynching at EOR");
              }
          }
        moded->panic = 0;
      }
    else
      {
        if (P_spec_level (pads)) 
          {
            return P_ERR;
          }
        moded->panic = 0;
        P_io_getLocE (pads,&(ted.loc),0);
        PDCI_report_err (pads,P_LEV_INFO,&(ted.loc),P_AT_EOR,"Found EOF when searching for EOR");
      }
  }
  return ((moded->nerr)==0) ? P_OK : P_ERR;
}
Perror_t out_sum_data_line_read (P_t *pads,out_sum_data_line_em *em,out_sum_data_line_ed *ed,out_sum_data_line *rep)
{
  out_sum_data_line tmprep={{{0},{0},{(enum dib_pn_vbar_tag_e) 0,{{0}}},{(enum dib_pn_vbar_tag_e) 0,{{0}}},{0},{(enum dib_pn_vbar_tag_e) 0,{{0}}},{(enum dib_pn_vbar_tag_e) 0,{{0}}}},{0},{0,0,0},{{(enum opt_auint32_vbar_tag_e) 0,{{0}}},{(enum opt_auint32_vbar_tag_e) 0,{{0}}},{(enum opt_auint64_vbar_tag_e) 0,{{0}}},{0},0}};
  out_sum_data_line *modrep=rep;
  out_sum_data_line_em tmpem={(enum Pbase_em_e) ((Pbase_em) 0),{(enum Pbase_em_e) 0,{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}}},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0}},{(enum Pbase_em_e) 0,(enum Pbase_em_e) 0},(enum Pbase_em_e) 0}};
  out_sum_data_line_em *modem=em;
  out_sum_data_line_ed tmped={0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,0,{{0,0,0},{0,0,0}},0,0,0,0,0},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}},0,{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}},{0,(enum PerrCode_t_e) 0,{{0,0,0},{0,0,0}}}}};
  out_sum_data_line_ed *moded=ed;
  if (!pads) 
    {
      P_WARN (&Pdefault_disc,"out_sum_data_line_read: null pads parameter.");
      return P_ERR;
    }
  if (!(pads->disc)) 
    {
      P_WARN (&Pdefault_disc,"out_sum_data_line_read: null pads->disc.");
      return P_ERR;
    }
  P_TRACE (pads->disc,"out_sum_data_line_read called.");
  if (!((pads->disc)->io_disc)) 
    {
      P_WARN (pads->disc,"out_sum_data_line_read: IO discipline not installed.");
      return P_ERR;
    }
  if (!modrep) 
    {
      modrep = (&tmprep);
    }
  if (!modem) 
    {
      modem = (&tmpem);
    }
  if (!moded) 
    {
      moded = (&tmped);
    }
  return out_sum_data_line_read_internal (pads,modem,moded,modrep);
}
Perror_t out_sum_data_line_acc_init (P_t *pads,out_sum_data_line_acc *acc)
{
  int nerr=0;
  if (P_ERR==out_sum_fixed1_acc_init (pads,&(acc->f1))) 
    {
      nerr++;
    }
  if (P_ERR==do_ev_count_acc_init (pads,&(acc->c))) 
    {
      nerr++;
    }
  if (P_ERR==eventSeq_acc_init (pads,&(acc->events))) 
    {
      nerr++;
    }
  if (P_ERR==out_sum_fixed2_acc_init (pads,&(acc->f2))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_data_line_acc_reset (P_t *pads,out_sum_data_line_acc *acc)
{
  int nerr=0;
  if (P_ERR==out_sum_fixed1_acc_reset (pads,&(acc->f1))) 
    {
      nerr++;
    }
  if (P_ERR==do_ev_count_acc_reset (pads,&(acc->c))) 
    {
      nerr++;
    }
  if (P_ERR==eventSeq_acc_reset (pads,&(acc->events))) 
    {
      nerr++;
    }
  if (P_ERR==out_sum_fixed2_acc_reset (pads,&(acc->f2))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_data_line_acc_cleanup (P_t *pads,out_sum_data_line_acc *acc)
{
  int nerr=0;
  if (P_ERR==out_sum_fixed1_acc_cleanup (pads,&(acc->f1))) 
    {
      nerr++;
    }
  if (P_ERR==do_ev_count_acc_cleanup (pads,&(acc->c))) 
    {
      nerr++;
    }
  if (P_ERR==eventSeq_acc_cleanup (pads,&(acc->events))) 
    {
      nerr++;
    }
  if (P_ERR==out_sum_fixed2_acc_cleanup (pads,&(acc->f2))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_data_line_acc_add (P_t *pads,out_sum_data_line_acc *acc,out_sum_data_line_ed *ed,out_sum_data_line *rep)
{
  int nerr=0;
  if (P_ERR==out_sum_fixed1_acc_add (pads,&(acc->f1),&(ed->f1),&(rep->f1))) 
    {
      nerr++;
    }
  if (P_ERR==do_ev_count_acc_add (pads,&(acc->c),&(ed->c),&(rep->c))) 
    {
      nerr++;
    }
  if (P_ERR==eventSeq_acc_add (pads,&(acc->events),&(ed->events),&(rep->events))) 
    {
      nerr++;
    }
  if (P_ERR==out_sum_fixed2_acc_add (pads,&(acc->f2),&(ed->f2),&(rep->f2))) 
    {
      nerr++;
    }
  return (nerr==0) ? P_OK : P_ERR;
}
Perror_t out_sum_data_line_acc_report_internal (P_t *pads,Sfio_t *outstr,char const *prefix,char const *what,int nst,out_sum_data_line_acc *acc)
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
      what = "struct out_sum_data_line";
    }
  PDCI_nst_prefix_what (outstr,&nst,prefix,what);
  sfprintf (outstr,"\n[Describing each field of %s]\n",prefix);
  sfprintf (tmpstr,"%s.f1",prefix);
  if (P_ERR==out_sum_fixed1_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->f1))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.c",prefix);
  if (P_ERR==do_ev_count_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->c))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.events",prefix);
  if (P_ERR==eventSeq_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->events))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfprintf (tmpstr,"%s.f2",prefix);
  if (P_ERR==out_sum_fixed2_acc_report_internal (pads,outstr,sfstruse (tmpstr),0,nst,&(acc->f2))) 
    {
      sfstrclose (tmpstr);
      return P_ERR;
    }
  sfstrclose (tmpstr);
  return P_OK;
}
Perror_t out_sum_data_line_acc_report (P_t *pads,char const *prefix,char const *what,int nst,out_sum_data_line_acc *acc)
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
  if (!((pads->disc)->errorf)) 
    {
      return P_OK;
    }
  result = out_sum_data_line_acc_report_internal (pads,outstr,prefix,what,nst,acc);
  if (P_OK==result) 
    {
      ((pads->disc)->errorf) (0,0,"%s",sfstruse (outstr));
    }
  sfstrclose (outstr);
  return result;
}
Perror_t out_sum_data_line_init (P_t *pads,out_sum_data_line *rep)
{
  if ((!pads)||(!rep)) 
    return P_ERR;
  eventSeq_init (pads,&(rep->events));
  return P_OK;
}
Perror_t out_sum_data_line_ed_init (P_t *pads,out_sum_data_line_ed *ed)
{
  if ((!pads)||(!ed)) 
    return P_ERR;
  eventSeq_ed_init (pads,&(ed->events));
  return P_OK;
}
Perror_t out_sum_data_line_cleanup (P_t *pads,out_sum_data_line *rep)
{
  if ((!pads)||(!rep)) 
    return P_ERR;
  eventSeq_cleanup (pads,&(rep->events));
  return P_OK;
}
Perror_t out_sum_data_line_ed_cleanup (P_t *pads,out_sum_data_line_ed *ed)
{
  if ((!pads)||(!ed)) 
    return P_ERR;
  eventSeq_ed_cleanup (pads,&(ed->events));
  return P_OK;
}

