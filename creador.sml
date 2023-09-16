(*
Tarea 2
lenguajes de programación
Duan Espinoza
2019079490
*)

(* Función para obtener una línea de texto del usuario *)
fun getLine () = Option.getOpt (TextIO.inputLine TextIO.stdIn, "");


(* Función para dividir una cadena de texto en una lista de cadenas *)
fun splitString separator text = String.tokens (fn c => c = separator) text;


(* Función para eliminar saltos de línea de una cadena de texto *)
fun removeNewlines text = String.translate (fn #"\n" => "" | c => String.str c) text;


(* Función para limpiar un archivo (crear uno nuevo) *)
fun clearFile path = let
    val fd = TextIO.openOut path
    val _ = TextIO.output(fd, "NOMBRE,PARADIGMA,PERMITE_DESARROLLO_MOVIL,PERMITE_DESARROLLO_WEB,RATING_PRINCIPAL,RATING_EMPLEOS\n")
    val _ = TextIO.closeOut fd
in
    ()
end;


(* Función para verificar si una cadena es un entero *)
fun isInteger str = case Int.fromString str of
    NONE => false
  | _ => true;


(* Función para verificar si una cadena es un número real *)
fun isReal str = case Real.fromString str of
    NONE => false
  | _ => true;



(* Función auxiliar para verificar si una cadena está compuesta solo de espacios en blanco *)
fun isWhitespace str =
    case String.tokens Char.isSpace str of
        [] => true
      | _  => false;


(* Función para solicitar un valor no numérico al usuario *)
fun requestNonNumericValue prompt = let
    val _ = print (prompt ^ ": ")
    val value = removeNewlines (getLine ())
in
    if String.size value = 0 orelse isWhitespace value then
        value
    else
        if isInteger value orelse isReal value then
            let
                val _ = print "\nIngrese un valor no numérico válido."
            in
                requestNonNumericValue prompt
            end
        else
            value
end;


(* Función para solicitar un valor numérico al usuario *)
fun requestNumericValue prompt = let
    val _ = print (prompt ^ ": ")
    val value = removeNewlines (getLine ())
in
    if isInteger value orelse isReal value then
        value
    else
        let
            val _ = print "\nIngrese un valor numerico valido."
        in
            requestNumericValue prompt
        end
end;


(* Función para agregar un nuevo lenguaje al índice *)
fun addLanguageToIndex path = let
    val fd = TextIO.openAppend path

    val name = requestNonNumericValue "Ingrese el nombre del lenguaje"
    val paradigm = requestNonNumericValue "Ingrese el paradigma del lenguaje"
    val allowsMobileDevelopment = requestNonNumericValue "¿Permite desarrollo movil? (si/no)"
    val allowsWebDevelopment = requestNonNumericValue "¿Permite desarrollo web? (si/no)"
    val principalRating = requestNumericValue "Ingrese el rating principal"
    val jobRatings = requestNumericValue "Ingrese el rating por empleos"

    val _ = TextIO.output(fd, name ^ "," ^ paradigm ^ "," ^ allowsMobileDevelopment ^ "," ^ allowsWebDevelopment ^ "," ^ principalRating ^ "," ^ jobRatings ^ "\n")
    val _ = TextIO.closeOut fd
in
    ()
end;


(* Función para mostrar el menú principal *)
fun printMainMenu () = let
    val _ = print "\nMenu Principal:\n"
    val _ = print "1. Agregar un nuevo lenguaje al indice\n"
    val _ = print "2. Limpiar el indice\n"
    val _ = print "0. Salir\n"
in
    ()
end;


(* Función para procesar la opción seleccionada *)
fun processOption path option = case option of
    "1" => addLanguageToIndex path
  | "2" => clearFile path
  | "0" => TextIO.output(TextIO.stdOut, "Gracias por usar el programa. ¡Hasta luego!\n")
  | _ => TextIO.output(TextIO.stdOut, "Opcion no valida. Por favor, seleccione una opcion valida.\n");


(* ... Código previo ... *)

(* Función para obtener la ruta del archivo del índice desde el usuario *)
fun getPathFromUser() =
    let
        val _ = print "Ingrese la ruta del archivo del indice: "
    in
        removeNewlines (getLine ())
    end;

(* Función para manejar la opción 1: Agregar un nuevo lenguaje al índice *)
fun handleOption1 path = addLanguageToIndex path;

(* Función para manejar la opción 2: Limpiar el índice *)
fun handleOption2 path = clearFile path;

(* Función para manejar la opción 0: Salir del programa *)
fun handleOption0 () = TextIO.output (TextIO.stdOut, "Gracias por usar el programa. ¡Hasta luego!\n");

(* Función principal del programa *)
fun main () =
    let
        val _ = print "Bienvenido al Gestor de Lenguajes.\n"
        val path = getPathFromUser ()
    in
        clearFile path; (* Limpiar el archivo del índice al inicio *)
        while true do
            let
                val _ = printMainMenu ()
                val option = requestNumericValue "Ingrese el numero de opcion deseada"
            in
                case option of
                    "1" => handleOption1 path
                  | "2" => handleOption2 path
                  | "0" => handleOption0 ()
                  | _   => TextIO.output (TextIO.stdOut, "Opcion no valida. Por favor, seleccione una opcion valida.\n")
            end
        end;


(* Ejecutar la función main para iniciar el programa *)
val _ = main ();
