#!/usr/bin/gawk -f
{
   print NR, ": ", $0;
}
/* para cada linha adicionar uma linha em branco caso nao exista. ou seja \n => \n\n e \n\n => \n\n */