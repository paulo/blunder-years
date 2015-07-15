#!/usr/bin/gawk -f
END{
    print NR;
}
/* para cada linha adicionar uma linha em branco caso nao exista. ou seja \n => \n\n e \n\n => \n\n */