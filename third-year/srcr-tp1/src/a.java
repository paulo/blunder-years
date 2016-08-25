import se.sics.jasper.Jasper;
import se.sics.jasper.SICStus;
import se.sics.jasper.SPPredicate;
import se.sics.jasper.SPQuery;
import se.sics.jasper.SPTerm;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author ruioliveiras
 */
public class a {

    public static void main(String[] args) {

        SICStus sp;
        SPPredicate pred;
        SPTerm from, to, way;
        SPQuery query;
        int i;

        try {
            sp = new SICStus(args, null);

            sp.load("pt1.pl");

            pred = new SPPredicate(sp, "connected", 4, "");
            to = new SPTerm(sp, "Orebro");
            from = new SPTerm(sp, "Stockholm");
            way = new SPTerm(sp).putVariable();

            query = sp.openQuery(pred, new SPTerm[]{from, to, way, way});

            while (query.nextSolution()) {
                System.out.println(way.toString());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }
}
