#include <training.p>

Precord Pstruct entry_t {
        Puint16_FW(:4:) type;
        Pwhite space1;
        Puint16_FW(:4:) id;
        Pwhite space2;
        Pfloat32 value1;
        Pwhite space3;
        Pfloat32 value2;
        Pwhite space4;
        Pfloat32 value3;
        Pwhite space5;
};

int setConsume(int* consume, int v){
 *consume = v;
 return 1;
};

/- the following is pretty convoluted ... what is consume? free var?
  
Parray subseq_t {
        entry_t[]: Pended(Pparsecheck(current>0 &&
                (elts[current].type!=elts[current-1].type || 
                 elts[current].id!=elts[current-1].id+1) && 
                setConsume(&consume, 0)));
} Pwhere {
        Pforall ( i Pin [0..length-2]: elts[i].type == elts[i+1].type &&
                elts[i].id+1 == elts[i+1].id);
};

Psource Parray entries_t {
        subseq_t[];
};                       
