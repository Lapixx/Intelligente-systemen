Voor opgave 5 gebruiken we software voor Inductive Logic Programming. Die software heet Aleph (http://www.cs.ox.ac.uk/activities/machlearn/Aleph/aleph.html).
Deze software werkt echter niet met SWIprolog. Daarom gebruiken we een andere Prolog interpreter: YAP (Yet Another Prolog, http://www.dcc.fc.up.pt/~vsc/Yap/)

Om YAP op de practicumcomputers te kunnen draaien, volg je de volgende stappen. Deze stappen zullen ook op je eigen computer werken, kies dan zelf je map.

1) Pak het pakketje uit (bijvoorbeeld in je U-schijf, je hebt dan Yap in: U:/Yap/).
2) Open "OpenYAP.bat" in de lib folder (of start YAP zelf op uit de commandline, maar zorg wel dat je dit vanuit de lib folder doet). We zijn nu in een Prolog-environment, net als SWI-Prolog.
3) Laad Aleph: "[aleph]."
4) Gebruik "read_all(grandfather)." om de grandfather bestanden in te lezen.
5) Gebruik "induce." om de gewenste regels af te leiden.