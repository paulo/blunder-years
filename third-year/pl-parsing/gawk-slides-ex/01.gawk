#!/usr/bin/gawk -f
{
    if(NF != 0){
        print;
        print ""; /*Wtf? the print is a println? (also prints \n) */
    } else {
        /*print;*/
    }

}
/* para cada linha adicionar uma linha em branco caso nao exista. ou seja \n => \n\n e \n\n => \n\n */
