functie som(Reeks a) : Getal {

    Getal tijdelijke_som = 0;
    voor (Getal i = 0; i < a.lengte(); i++) {
        tijdelijke_som = tijdelijke_som + a[i];
    }

    geefterug tijdelijke_som;
}

functie toevoeging (Getal a, Getal b) : Getal {
    geefterug a + b;
}

functie hoofd() {
    Reeks lijst = [1,2,3];

    afdrukken("De som van de lijst plus 5 is ");
    afdrukken(toevoeging(som(lijst),5));
    afdrukken("\n");
}