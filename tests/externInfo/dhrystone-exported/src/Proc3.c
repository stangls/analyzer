#include "dry.h"

void Proc3(RecordPtr *PtrParOut)
{
        if (PtrGlb != NULL){
                 counter+=31;
                *PtrParOut = PtrGlb->PtrComp;
        }
        else{
                counter+=35;
                IntGlob = 100;
        }
        Proc7(10, IntGlob, &PtrGlb->IntComp);
}
