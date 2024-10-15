# encuestar 1.0.0

# La primer version de la paquetería agrupa en una sola clase los posibles resultados que se han implementado a lo largo de 3 años de trabajo.

# Tercer sprint

# El tercer sprint arregla un error fundamental en la construccion del diseno muestral. Los rangos de edades que se necesitan son de 18 a 24, de 25 a 59 y 60 y mas. Aunque el código estaba bien implementado, las etiquetas de la variable que contiene esa inforamción estaban incorrectas pues decían "18 a 24", "25 a 60" y "60 y mas" lo que genera una ambiguedad. Este error se herda desde el paquete `muestrear` utilizado para generar los disenos y muestras utilizados en cada encuesta. El arreglo implica la modificación de ambas paqueterías y, en `encuestar` afecta a todo el proceso de generación de diseno muestral y al código contenido en la aplicación de monitoreo. Esto implica que cualquier diseño muestral generado antes de esta modificación (Diciembre 2023) no podrá ser utilizado en la paquetería a partir de esta actualización. Si se busca reproducir los resultados de cualquier encuesta anterior a esa fecha, existe una rama llamada diseno_anterior que conserva ese error.

# Segundo sprint

# El segundo sprint se enfoca en el script de la aplicación `Shiny` desplegada en el modo de encuesta tipo INE. Se reorganiza la UI, se comenta el código fuente y el cambio más relevante es el cambio del framework de `shinydashboard` a `bslib`.

# Primer sprint

# El primer sprint de la versión de desarrollo tiene como objetivo consolidar los resultados que es capaz de generar. Los método posibles estaban en dos clasez diferentes: 'preguntas' y 'regiones'. Se construye un mapa de todo los resultados posibles y ambos pasan a formar parte de una sola clase llamada 'Graficas'. Se reconstruyen la mayoría de los métodos y se comienzan a factorizar los métodos contenidos en dicha clase.

# encuestar 0.9000

# La paquetería encuestar nace con la necesidad de procesar datos de encuestas de opinión pública levantadas en domicilio. Inicialmente es capaz de mostrar resultados de una o varias variables en forma de visualizaciones usando `ggplot2`. También contiene la primera versión de la app de auditoría usando el framework de shinydashboard.
