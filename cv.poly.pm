#lang pollen

◊CVheader{
  ◊CVname{Name Surname}
  ◊CVaddress{Street Something 32, Somewhere}
  ◊CVphone{555 3030}, ◊CVemail{mail@example.com}
}

◊CVsection[#:title "Education"]{
  ◊CVobject[#:period "2000-2003"]{Bachelor at Padova University}
  ◊CVitems{
    ◊CVitem{Degree on Information Engineering}
    ◊CVitem{Thesis analyzing NASA Kepler observations with ◊sl{machine learning} tools}
    ◊CVitem{Pubblication on IEEE MOCAST

            Lovisotto, ◊sl{et alia}, ◊url[
                #:link "http://ieeexplore.ieee.org/document/7937674/"]{
                Cell traffic prediction using joint spatio-temporal information},

            ◊sl{6th International Conference on Modern Circuits and Systems Technologies}, 2017}}

  ◊CVobject[#:period "2003--2005"]{Relevant master courses}
  ◊CVitems{
    ◊CVitem[
      #:tag ◊url[#:link "http://en.didattica.unipd.it/off/2016/LM/IN/IN0524/000ZZ/INN1027895/N0"]{
      Telecommunication Networks}]{Andrea Zanella}
    ◊CVitem[
      #:tag ◊url[#:link "http://en.didattica.unipd.it/off/2016/LM/IN/IN0524/000ZZ/INP6075439/N0"]{
      Wireless Communications}]{Michele Rossi}
    ◊CVitem[
      #:tag ◊url[#:link "http://en.didattica.unipd.it/off/2016/LM/IN/IN0521/000ZZ/INP3049939/N0"]{
      Network Modeling}]{Michele Zorzi}
    ◊CVitem[
      #:tag ◊url[#:link "http://en.didattica.unipd.it/off/2016/LM/IN/IN0524/000ZZ/INP6075820/N0"]{
      Antennas and Wireless Propagation}]{Marco Santagiustina}
  }

  ◊CVobject{Miscellaneous}
  ◊CVitems{
    ◊CVitem[#:tag "Programming languages"]{
      Python, R, Matlab, C++, Rust, Racket}
    ◊CVitem[#:tag "Data analysis and machine learning"]{
      Pandas, Scikit-learn, R dataframes}
    ◊CVitem[#:tag "Foreign languages"]{
      English B2, certified by Padova university}}
}

◊CVsection[#:title "Work experiences"]{
◊CVobject[#:period "2004"]{Internship}
◊CVitems{
  ◊CVitem{IT support of cardiac ward technicians}
  ◊CVitem{Assistence of medical staff: MS Windows, MS Office, Ubuntu GNU/Linux server}
}
