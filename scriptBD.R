library(RMySQL)
options(encoding = 'UTF-8')



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
  Correo VARCHAR(50) UNIQUE NOT NULL,
  Contraseña VARCHAR(150) NOT NULL,
  Cambiar_contraseña Boolean NOT NULL DEFAULT 1
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


# Creo tabla Intento_Cuestionario
dbSendQuery(bd, "CREATE TABLE Intento_Cuestionario (
  ID_intento VARCHAR(10) PRIMARY KEY,
  Puntuacion DECIMAL(3,2) NOT NULL,
  Fecha DATE NOT NULL
)")

## Antes de insertar se deberá comprobar si al parsear la pregunta 
## es una pregunta correcta
## Valorar si cambiar a text tanto enunciado como respuestas. Puede incluir codigo
## LATEX
## Si conozco todos los tipos de preguntas posibles sustituir TIPO_PREGUNTA 
## VARCHAR por ENUM
dbSendQuery(bd, "CREATE TABLE Preguntas(
  Id_pregunta VARCHAR(10) PRIMARY KEY,
  Tipo_Pregunta VARCHAR(10) NOT NULL,
  Puntuacion DECIMAL(3,2) NOT NULL,
  Penalizacion DECIMAL(3,2) NOT NULL,
  Enunciado   VARCHAR(500) NOT NULL,
  Respuesta_1   VARCHAR(500) NOT NULL,
  Respuesta_2   VARCHAR(500) NOT NULL,
  Respuesta_3   VARCHAR(500),
  Respuesta_4   VARCHAR(500),
  Respuesta_5   VARCHAR(500),
  Soluciones    JSON
)")

dbSendQuery(bd, "CREATE TABLE Temas_Preguntas(
  Temas_ID VARCHAR(10),
  Preguntas_ID VARCHAR(10), 
  PRIMARY KEY (Temas_ID,Preguntas_ID)
)")

dbSendQuery(bd, "CREATE TABLE Usuarios_Asignaturas (
  Usuarios_ID VARCHAR(10),
  Asignaturas_ID VARCHAR(10),
  Camino_Aprendizaje JSON, 
  PRIMARY KEY (Usuarios_ID, Asignaturas_ID)
)")

dbSendQuery(bd, "CREATE TABLE Intento_Cuestionario_Preguntas(
  Intento_ID VARCHAR(10),
  Preguntas_ID VARCHAR(10),
  Respuesta_Alumno INT,
  PRIMARY KEY (Intento_ID, Preguntas_ID)
)")





# Añado FKeys para modelar las relaciones entre tablas
dbSendQuery(bd, "ALTER TABLE Usuarios_Asignaturas 
            ADD FOREIGN KEY (Usuarios_ID) REFERENCES Usuarios (ID_usuario)
            ON DELETE CASCADE")

dbSendQuery(bd, "ALTER TABLE Usuarios_Asignaturas
            ADD FOREIGN KEY (Asignaturas_ID) REFERENCES Asignaturas (Codigo_asignatura)
            ON DELETE CASCADE")

dbSendQuery(bd, "ALTER TABLE Intento_Cuestionario_Preguntas
            ADD FOREIGN KEY (Intento_ID) REFERENCES Intento_Cuestionario (ID_intento)
            ON DELETE CASCADE")

dbSendQuery(bd, "ALTER TABLE Intento_Cuestionario_Preguntas
            ADD FOREIGN KEY (Preguntas_ID) REFERENCES Preguntas (ID_pregunta)
            ON DELETE CASCADE")

dbSendQuery(bd, "ALTER TABLE Temas_Preguntas
            ADD FOREIGN KEY (Temas_ID) REFERENCES Temas (Codigo_tema)
            ON DELETE CASCADE")

dbSendQuery(bd, "ALTER TABLE Temas_Preguntas
            ADD FOREIGN KEY (Preguntas_ID) REFERENCES Preguntas (ID_pregunta)
            ON DELETE CASCADE")

dbSendQuery(bd, "ALTER TABLE Asignaturas
            ADD Codigo_tema VARCHAR(10) NOT NULL,
            ADD FOREIGN KEY (Codigo_tema) REFERENCES Temas (Codigo_tema)
            ON DELETE CASCADE")


dbSendQuery(bd, "ALTER TABLE Intento_Cuestionario
            ADD Usuario_ID VARCHAR(10) NOT NULL,
            ADD FOREIGN KEY (Usuario_ID) REFERENCES Usuarios (ID_Usuario)
            ON DELETE CASCADE")

#dbSendQuery(bd, "Drop database appcuestionarios")


dbDisconnect(bd)
