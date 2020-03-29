# COVID19-modelling
Existen 3 datasets que contienen:  
    **1** - datos sobre casos, altas, fallecidos, hospitalizados y en UCI,  
    **2** - datos por franja de edad y sexo de los casos confirmados, hospitalizados, ingresados en UCI y fallecidos  
    **3** - y datos sobre el número de camas de UCI por CCAA.  
   
Para ejecutar el dataset descrito en (1) hay que tener en cuenta las siguientes dependencias:  
    **1** - los scrips *altas_study.R*, *casos_study.R*, *fallecidos_study.R*, *hospitalizados_study.R* y *uci_study.R* cargan y modifican los datos para mejorar su manipulación. Nótese que los datos se extraen de un repositorio público actualizado día a día con los datos del Ministerio de Sanidad.  
    **2** - Estos scripts, a su vez, dependen de *my_functions.R*, donde se contienen funciones necesarias para llevar a cabo las anteriores modificaciones.   
    **3** - Finalmente, ejecutando el script *navigation_list* se obtiene una lista de datasets con las variables de interés (fecha, casos, altas, fallecido, hospitalizados y en UCI). Por lo tanto, el script proncipal sería este último, siendo los anteriores necesarios para su creación.  
    
Los datasets (2) y (3) no presentan dependencias, dado que son extraidos del repositorio y presentados tal cual vienen. Viene cargados en los scripts *rango_edad_study.R* y *camas_uci.R* respectivamente.

 
