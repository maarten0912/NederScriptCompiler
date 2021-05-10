grammar SimpleArithmeticAttr;

import SimpleArithmeticVocab;

@members {
    private int getValue(String text) {
        return Integer.parseInt(text);
    }
}

t returns [ Type type ]
     : t0=t HAT t1=t
       {
         if ($t1.type == Type.NUM && $t0.type != Type.BOOL) {
           $type = $t0.type;
         } else {
           $type = Type.ERR;
         }
       }
     | t0=t PLUS t1=t
       {
         if ($t0.type == $t1.type) {
           $type = $t0.type;
         } else {
           $type = Type.ERR;
         }
       }
     | t0=t EQUALS t1=t
       {
         if ($t0.type == $t1.type) {
           $type = Type.BOOL;
         } else {
           $type = Type.ERR;
         }
       }
     | LPAR t0=t RPAR
       { $type = $t0.type; }
     | NUMBER
       { $type = Type.NUM; }
     | BOOL
       { $type = Type.BOOL; }
     | STR
       { $type = Type.STR; }
     ;
