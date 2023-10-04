library(RMySQL)

# Conexión con la base de datos 
bd <- dbConnect(MySQL(), user = "root", password = "root", 
                #dbname = "appcuestionarios",
                host = "localhost")

# Creo la base de datos
# dbSendQuery(bd, "CREATE DATABASE appcuestionarios;")

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


# Creo tabla Intento_Cuestionario
dbSendQuery(bd, "CREATE TABLE Intento_Cuestionario (
  ID_intento VARCHAR(10) PRIMARY KEY,
  Puntuacion DECIMAL(3,2) NOT NULL,
  Fecha DATE NOT NULL
)")

dbSendQuery(bd, "CREATE TABLE Preguntas(
  Id_pregunta VARCHAR(10) PRIMARY KEY,
  Enunciado   VARCHAR(500) NOT NULL,
  Respuesta_1   VARCHAR(500) NOT NULL,
  Respuesta_2   VARCHAR(500) NOT NULL,
  Respuesta_3   VARCHAR(500),
  Respuesta_4   VARCHAR(500)
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
            ADD Codigo_tema VARCHAR(10) NOT NULL,
            ADD FOREIGN KEY (Codigo_tema) REFERENCES Temas (Codigo_tema)")

dbSendQuery(bd, "ALTER TABLE Asignaturas
            ADD Codigo_Grupo VARCHAR(10) NOT NULL,
            ADD FOREIGN KEY (Codigo_Grupo) REFERENCES Grupos (Codigo_grupo)")


dbSendQuery(bd, "ALTER TABLE Usuarios
            ADD ID_intento VARCHAR(10) NOT NULL,
            ADD FOREIGN KEY (ID_intento) REFERENCES Intento_Cuestionario (ID_intento)")

#dbSendQuery(bd, "Drop database appcuestionarios")



dbDisconnect(bd)
