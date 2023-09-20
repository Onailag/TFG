library(RMySQL)

# Conexión con la base de datos 
bd <- dbConnect(MySQL(), user = "root", password = "root", 
                #dbname = "appcuestionarios",
                host = "localhost")

# Creo la base de datos
dbSendQuery(bd, "CREATE DATABASE appcuestionarios;")

# Uso la base de datos
dbSendQuery(bd, "USE appcuestionarios")

# Creo tabla Usuarios
dbSendQuery(bd, "CREATE TABLE Usuarios (
  ID_usuario VARCHAR(10) PRIMARY KEY,
  Nombre VARCHAR(50) NOT NULL,
  Rol ENUM ('alumno', 'profesor', 'administrador') NOT NULL,
  Email VARCHAR(50) UNIQUE NOT NULL,
  Password VARCHAR(150) NOT NULL
)")

# Creo tabla Asignaturas
dbSendQuery(bd, "CREATE TABLE Asignaturas (
  Codigo_asignatura VARCHAR(10) PRIMARY KEY,
  Nombre VARCHAR(50) NOT NULL,
  Descripcion TEXT,
  Curso INT NOT NULL
)")

# Creo tabla Temas
dbSendQuery(bd, "CREATE TABLE Temas (
  Codigo_tema VARCHAR(10) PRIMARY KEY,
  Nombre VARCHAR(50) NOT NULL,
  Descripcion TEXT
)")

# Creo tabla Grupos
dbSendQuery(bd, "CREATE TABLE Grupos (
  Codigo_grupo VARCHAR(10) PRIMARY KEY
)")


# Creo tabla Cuestionarios
dbSendQuery(bd, "CREATE TABLE Cuestionarios (
  ID_cuestionario VARCHAR(10) PRIMARY KEY,
  Puntuacion DECIMAL(3,2) NOT NULL,
  Fecha DATE NOT NULL
)")


# Creo la tabla Usuarios_Grupos para modelar relacion M:M
# Representa las inscripciones de un Usuario en un Grupo.
dbSendQuery(bd, "CREATE TABLE Usuarios_Grupos (
  Usuarios_ID VARCHAR(10),
  Grupos_ID VARCHAR(10),
  PRIMARY KEY (Usuarios_ID, Grupos_ID)
)")



# Añado FKeys para modelar las relaciones entre tablas
dbSendQuery(bd, "ALTER TABLE Usuarios_Grupos 
            ADD FOREIGN KEY (Usuarios_ID) REFERENCES Usuarios (ID_usuario)")

dbSendQuery(bd, "ALTER TABLE Usuarios_Grupos
            ADD FOREIGN KEY (Grupos_ID) REFERENCES Grupos (Codigo_grupo)")

dbSendQuery(bd, "ALTER TABLE Asignaturas
            ADD FOREIGN KEY (Codigo_asignatura) REFERENCES Temas (Codigo_tema)")


dbSendQuery(bd, "ALTER TABLE Asignaturas
            ADD FOREIGN KEY (Codigo_asignatura) REFERENCES Grupos (Codigo_grupo)")

dbSendQuery(bd, "ALTER TABLE Temas
            ADD FOREIGN KEY (Codigo_tema) REFERENCES Cuestionarios (ID_cuestionario)")

dbSendQuery(bd, "ALTER TABLE Usuarios
            ADD FOREIGN KEY (ID_usuario) REFERENCES Cuestionarios (ID_cuestionario)")

#dbSendQuery(bd, "Drop database appcuestionarios")

####
# Errores al insertar 
# Error in .local(conn, statement, ...) : 
# could not run statement: Cannot add or update a child row: a foreign key constraint fails (`appcuestionarios`.`usuarios`, 
# CONSTRAINT `usuarios_ibfk_1` FOREIGN KEY (`ID`) REFERENCES `cuestionarios` (`ID`))
#
#
# Revisar relaciones.
#
###

dbDisconnect(bd)
