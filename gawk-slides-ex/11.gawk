#!/usr/bin/gawk -f
{
    lines[NR] = $0;
}

END{
    for(i = NR - 1; i >= 0; i--){
        print lines[i];
    }

}

/* para cada linha adicionar uma linha em branco caso nao exista. ou seja \n => \n\n e \n\n => \n\n */