#define GLX_STR_MATCH(p,s) (strcmp((p),(s)) == 0)

void bar_init_perm(PDCI_node_t *node){
}

void bar_init_temp(PDCI_node_t *node){
}

PDCI_node_t *bar_kth_child (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  bar *rep=(bar *) (self->rep);
  bar_pd *pd=(bar_pd *) (self->pd);
  bar_m *m=(bar_m *) (self->m);
  PDCI_node_t *result = 0;

  switch(idx){
  case 0: 
      // parse descriptor child
    PDCI_MK_TNODE (result, &PDCI_structured_pd_vtable,self,"pd",pd,"bar_children");
    break;
  case 1:
    PDCI_MK_NODE (result,&Pint16_vtable,self,"f1",&(m->f1),&(pd->f1),&(rep->f1),"element","bar_children");
    break;
  case 2:
    PDCI_MK_NODE (result,&Pint32_vtable,self,"f2",&(m->f2),&(pd->f2),&(rep->f2),"element","bar_children");
    break;
  case 3:
    PDCI_MK_NODE (result,&Pchar_vtable,self,"f3",&(m->f3),&(pd->f3),&(rep->f3),"element","bar_children");
    break;
  }
  return result;
}

PDCI_node_t *bar_kth_child_named (PDCI_node_t *self, PDCI_childIndex_t idx, const char *name)
{
  bar *rep=(bar *) (self->rep);
  bar_pd *pd=(bar_pd *) (self->pd);
  bar_m *m=(bar_m *) (self->m);
  PDCI_node_t *result = 0;

  // The index must be 0 as all field names are unique.
  if (idx != 0)
    return result;

  if (GLX_STR_MATCH(name,"pd")){
    // parse descriptor child  
    PDCI_MK_TNODE (result,&PDCI_structured_pd_vtable,self,"pd",pd,"bar_children");
  }else if (GLX_STR_MATCH(name,"f1")){
    PDCI_MK_NODE (result,&Pint16_vtable,self,"f1",&(m->f1),&(pd->f1),&(rep->f1),"element","bar_children");
  }else if (GLX_STR_MATCH(name,"f2")){
    PDCI_MK_NODE (result,&Pint32_vtable,self,"f2",&(m->f2),&(pd->f2),&(rep->f2),"element","bar_children");
  }else if (GLX_STR_MATCH(name,"f3")){
    PDCI_MK_NODE (result,&Pchar_vtable,self,"f3",&(m->f3),&(pd->f3),&(rep->f3),"element","bar_children");
  }

  return result;
}

PDCI_vtable_t const bar_vtable={bar_children,bar_kth_child,bar_kth_child_named,PDCI_error_typed_value,0};


PDCI_node_t *barArray_kth_child (PDCI_node_t *self, PDCI_childIndex_t idx)
{
  barArray *rep=(barArray *) (self->rep);
  barArray_pd *pd=(barArray_pd *) (self->pd);
  barArray_m *m=(barArray_m *) (self->m);
  PDCI_node_t *result = 0;

  switch(idx){
  case 0: // parse descriptor child 
    PDCI_MK_TNODE (result,&PDCI_sequenced_pd_vtable,self,"pd",pd,"barArray_children");
    break;
  case 1: // length field
    PDCI_MK_TNODE (result,&Puint32_val_vtable,self,"length",&(rep->length),"barArray_children");
    break;
  default: // now do elements
    idx -= 2;
    if (idx >= 0 && idx < rep->length)
      PDCI_MK_NODE (result,&bar_vtable,self,"elt",&(m->element),&(pd->elts)[idx],&(rep->elts)[idx],"element","barArray_children");
    break;
  }
  return result;
}

PDCI_node_t *barArray_kth_child_named (PDCI_node_t *self, PDCI_childIndex_t idx, const char *name)
{
  barArray *rep=(barArray *) (self->rep);
  barArray_pd *pd=(barArray_pd *) (self->pd);
  barArray_m *m=(barArray_m *) (self->m);
  PDCI_node_t *result = 0;

  if (GLX_STR_MATCH(name,"pd") && idx == 0){
  // parse descriptor child
  PDCI_MK_TNODE (result,&PDCI_sequenced_pd_vtable,self,"pd",pd,"barArray_children");
  }else if(GLX_STR_MATCH(name,"length") && idx == 0){
    PDCI_MK_TNODE (result,&Puint32_val_vtable,self,"length",&(rep->length),"barArray_children");
  }else if(GLX_STR_MATCH(name,"elt") && idx < rep->length){
    PDCI_MK_NODE (result,&bar_vtable,self,"elt",&(m->element),&(pd->elts)[idx],&(rep->elts)[idx],"element","barArray_children");
  }
  return result;
}

PDCI_vtable_t const barArray_vtable={barArray_children,barArray_kth_child,barArray_kth_child_named,PDCI_error_typed_value,0};
