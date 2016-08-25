#!/usr/bin/gawk -f
{
    if (!emBranco($0)){
        print $0;
    }
}

function emBranco(linha){
    return length(linha) < 1;
}


/* para cada linha adicionar uma linha em branco caso nao exista. ou seja \n => \n\n e \n\n => \n\n */