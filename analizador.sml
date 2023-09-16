(*
Tarea 2
lenguajes de programación
Duan Espinoza
2019079490
*)

open Order;
open IntRedBlackSet;



(* Función para obtener una línea de texto del usuario *)
fun getLine () = Option.getOpt (TextIO.inputLine TextIO.stdIn, "");


(* Función para dividir una cadena de texto en una lista de cadenas *)
fun splitString separator text = String.tokens (fn c => c = separator) text;


(* Función para eliminar saltos de línea de una cadena de texto *)
fun removeNewlines text = String.translate (fn #"\n" => "" | c => String.str c) text;



fun compareByRating ((_, _, _, _, rating1, _), (_, _, _, _, rating2, _)) =
    case Real.fromString rating1 of
        SOME r1 =>
            (case Real.fromString rating2 of
                SOME r2 => if r1 > r2 then GREATER else LESS
              | NONE => LESS)
      | NONE => LESS



(* Función de ordenación personalizada *)
fun customSort compareFunc lst =
    let
        fun insertionSort [] = []
          | insertionSort (x::xs) =
            let
                fun insert (y, []) = [y]
                  | insert (y, z::zs) =
                    if compareFunc (x, z) = LESS then
                        y::x::z::zs
                    else
                        z::insert(y, zs)
            in
                insert (x, insertionSort xs)
            end
    in
        insertionSort lst
    end;


(* Función para limpiar un archivo (crear uno nuevo) *)
fun clearFile path = let
    val fd = TextIO.openOut path
    val _ = TextIO.output(fd, "NOMBRE,PARADIGMA,PERMITE_DESARROLLO_MOVIL,PERMITE_DESARROLLO_WEB,RATING_PRINCIPAL,RATING_EMPLEOS\n")
    val _ = TextIO.closeOut fd
in
    ()
end;




(* Llama a la función customSort con los datos como argumento *)
val sortedData = customSort compareByRating data



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
    val _ = print "a. Mostrar top (ascendente por posición)\n"
    val _ = print "b. Mostrar top por rating por empleos (ascendente por rating empleo)\n"
    val _ = print "c. Detalles de lenguajes web\n"
    val _ = print "d. Detalles de lenguajes móviles\n"
    val _ = print "e. Cantidad de lenguajes por paradigma\n"
    val _ = print "f. Resumen\n"
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



(* Función para manejar la opción a: Mostrar top (ascendente por posición) *)
fun handleOptionA data =
    let
        
        val sortedData = customSort compareByRating data
        (* Mostrar el resultado en forma tabulada *)
        val _ = print "Top (ascendente por posición):\n"
        val _ = print "Posición | Nombre de Lenguaje | Rating Principal\n"
        val _ = print "--------------------------------------------\n"
        fun printLanguage (position, (name, _, _, _, rating, _)) =
            print (Int.toString position ^ "\t|\t" ^ name ^ "\t|\t" ^ rating ^ "\n")
        val _ = ListPair.appi printLanguage (sortedData, List.tabulate (length sortedData, fn i => i + 1))
    in
        ()
    end;



(* Función para manejar la opción b: Mostrar top por rating por empleos (ascendente por rating empleo) *)
fun handleOptionB data =
    let
        
        val sortedData = customSort compareByRating data
        (* Mostrar el resultado en forma tabulada *)
        val _ = print "Top por Rating por Empleos (ascendente por rating empleo):\n"
        val _ = print "Nombre de Lenguaje | Rating por Empleos\n"
        val _ = print "-------------------------------------\n"
        fun printLanguage (name, rating) =
            print (name ^ "\t|\t" ^ rating ^ "\n")
        val _ = List.app printLanguage sortedData
    in
        ()
    end;



(* Función para manejar la opción c: Detalles de lenguajes web *)
fun handleOptionC data =
    let
        
        val webLanguages = filterLanguagesByCategory data "web"
        (* Mostrar los detalles de lenguajes web en forma tabulada *)
        val _ = print "Detalles de Lenguajes Web:\n"
        val _ = print "Nombre de Lenguaje | Paradigma | Rating Principal | Rating por Empleos\n"
        val _ = print "-------------------------------------------------------------\n"
        fun printLanguage (name, paradigm, _, _, rating1, rating2) =
            print (name ^ "\t|\t" ^ paradigm ^ "\t|\t" ^ rating1 ^ "\t|\t" ^ rating2 ^ "\n")
        val _ = List.app printLanguage webLanguages
    in
        ()
    end;





(* Función para manejar la opción d: Detalles de lenguajes móviles *)
fun handleOptionD data =
    let
        
        val mobileLanguages = filterLanguagesByCategory data "movil"
        (* Mostrar los detalles de lenguajes móviles en forma tabulada *)
        val _ = print "Detalles de Lenguajes Móviles:\n"
        val _ = print "Nombre de Lenguaje | Paradigma | Rating Principal | Rating por Empleos\n"
        val _ = print "-------------------------------------------------------------\n"
        fun printLanguage (name, paradigm, _, _, rating1, rating2) =
            print (name ^ "\t|\t" ^ paradigm ^ "\t|\t" ^ rating1 ^ "\t|\t" ^ rating2 ^ "\n")
        val _ = List.app printLanguage mobileLanguages
    in
        ()
    end;




(* Función para manejar la opción e: Cantidad de lenguajes por paradigma *)
fun handleOptionE data =
    let
        val paradigm = requestNonNumericValue "Ingrese el paradigma a buscar"
        (* Filtra los lenguajes por paradigma *)
        val filteredLanguages = filterLanguagesByParadigm data paradigm
    in
        (* Muestra la información resultante de manera tabulada *)
        val _ = print ("Lenguajes en el paradigma '" ^ paradigm ^ "':\n")
        val _ = print "Nombre de Lenguaje | Paradigma | Rating Principal | Rating por Empleos\n"
        val _ = print "-------------------------------------------------------------\n"
        fun printLanguage (name, paradigm, _, _, rating1, rating2) =
            print (name ^ "\t|\t" ^ paradigm ^ "\t|\t" ^ rating1 ^ "\t|\t" ^ rating2 ^ "\n")
        val _ = List.app printLanguage filteredLanguages
    end;




(* Función para manejar la opción f: Resumen *)
fun handleOptionF data =
    let
        
        val highestMainRatingLanguage = getLanguageWithHighestMainRating data
        (* Obtener el lenguaje con el rating principal más bajo *)
        val lowestMainRatingLanguage = getLanguageWithLowestMainRating data
        (* Obtener el paradigma con más lenguajes *)
        val (paradigmWithMostLanguages, count) = getParadigmWithMostLanguages data
        (* Mostrar el resumen *)
        val _ = print "Resumen:\n"
        val _ = print "-------------------------------------------------------------\n"
        val _ = print "Lenguaje con el Rating Principal más Alto: " ^ #1 highestMainRatingLanguage ^ "\n"
        val _ = print "Rating Principal más Alto: " ^ #5 highestMainRatingLanguage ^ "\n"
        val _ = print "Lenguaje con el Rating Principal más Bajo: " ^ #1 lowestMainRatingLanguage ^ "\n"
        val _ = print "Rating Principal más Bajo: " ^ #5 lowestMainRatingLanguage ^ "\n"
        val _ = print "Paradigma con más Lenguajes: " ^ paradigmWithMostLanguages ^ "\n"
        val _ = print "Cantidad de Lenguajes en ese Paradigma: " ^ Int.toString count ^ "\n"
    in
        ()
    end;



(* Función para manejar la opción 0: Salir *)
fun handleOption0 () =
    let
        val _ = TextIO.output (TextIO.stdOut, "Gracias por usar el programa. ¡Hasta luego!\n")
    in
        (* Termina el programa *)
        raise Empty
    end;




(* Función para cargar los datos desde un archivo CSV *)
fun loadDataFromFile path =
    let
        val file = TextIO.openIn path
        val header = TextIO.inputLine file;  (* Leer la primera línea (encabezado) y descartarla *)
        val lines = TextIO.inputAll file;    (* Leer todas las líneas restantes *)
    in
        TextIO.closeIn file;
        (* Dividir las líneas en una lista de registros *)
        map (splitString ",") (String.tokens (fn c => c = #"\n") lines)
    end;

(* Función para obtener el lenguaje con el rating principal más alto *)
fun getLanguageWithHighestMainRating data =
    let
        fun maxByMainRating ([], currentMax) = currentMax
          | maxByMainRating (((_, _, _, rating1), (_, _, _, rating2)) :: rest, currentMax) =
            if Real.fromString rating1 > Real.fromString rating2 then
                maxByMainRating (rest, rating1)
            else
                maxByMainRating (rest, rating2)
    in
        maxByMainRating (data, "0.0")
    end;

(* Función para obtener el lenguaje con el rating principal más bajo *)
fun getLanguageWithLowestMainRating data =
    let
        fun minByMainRating ([], currentMin) = currentMin
          | minByMainRating (((_, _, _, rating1), (_, _, _, rating2)) :: rest, currentMin) =
            if Real.fromString rating1 < Real.fromString rating2 then
                minByMainRating (rest, rating1)
            else
                minByMainRating (rest, rating2)
    in
        minByMainRating (data, "10.0") (* Inicializar con un valor alto para que se reemplace *)
    end;

(* Función para obtener el paradigma con más lenguajes *)
fun getParadigmWithMostLanguages data =
    let
        
        fun countParadigms ([], counts) = counts
          | countParadigms (((_, paradigm, _, _), rest) :: xs, counts) =
            countParadigms (xs, if IntMap.member (counts, paradigm) then IntMap.add (counts, paradigm, 1) else counts)
        (* Encontrar el paradigma con la cuenta máxima *)
        fun findMaxParadigm counts = let
            fun maxPair ((paradigm1, count1), (paradigm2, count2)) =
                if count1 > count2 then
                    (paradigm1, count1)
                else
                    (paradigm2, count2)
        in
            IntMap.fold maxPair counts ("", 0)
        end
    in
        findMaxParadigm (countParadigms (data, IntMap.empty))
    end;

(* Función para filtrar lenguajes por paradigma *)
fun filterLanguagesByParadigm data paradigm =
    List.filter (fn ((_, p, _, _), _) => p = paradigm) data;

(* Función para filtrar lenguajes por categoría (desarrollo web o móvil) *)
fun filterLanguagesByCategory data category =
    List.filter (fn ((_, _, mobile, web, _, _), _) => (category = "web" andalso web = "si") orelse (category = "movil" andalso mobile = "si")) data;






(* Función principal *)
fun main () =
    let
        val _ = print "Bienvenido al Analizador de Índice IEEE Spectrum.\n"
        val path = getPathFromUser ()
        val data = loadDataFromFile path
    in
        (* El programa continuará ejecutándose hasta que el usuario elija salir (opción 0) *)
        while true do
            let
                val _ = printMainMenu ()
                val option = requestNonNumericValue "Ingrese la opción deseada"
            in
                case option of
                    "a" => handleOptionA data
                  | "b" => handleOptionB data
                  | "c" => handleOptionC data
                  | "d" => handleOptionD data
                  | "e" => handleOptionE data
                  | "f" => handleOptionF data
                  | "0" => handleOption0 ()
                  | _   => TextIO.output (TextIO.stdOut, "Opción no válida. Por favor, seleccione una opción válida.\n")
            end
        end;






(* Ejecutar la función main para iniciar el programa *)
val _ = main ();
