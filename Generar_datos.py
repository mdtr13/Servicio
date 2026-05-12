import os
import numpy as np
import itertools

def variacion_muiltitple(config_barrido, archivo_base="transmitancia_original.f"):
    """
    config_barrido: Diccionario que configura
                    como se van a variar los parametros
                    del fortran
                    {"parametro_1":(inicio, fin, pasos) , "param_2":(inicio, fin, pasos) , etc}

                    Ejemplo:

                    {"EA": (0, 1, 5), "TBA": (-1, 0, 1)}

                    Esta version va a variar;
                      "EA" de 0 a 1 en 5 pasos

                      "TBA" de -1 a 0 en 1 pasos
    """
    nombre_temp = "temp_simulacion.f"

    # A partir del diccionario de configuracion
    # tenemos que crear los intervalos de cada parametro
    # con su correspondiente inicio, fin y pasos

    # Devuelve la lista de nombres en el dicc
    nombres_params = list(config_barrido.keys())
    # Ej ==> ["EA", "TBA"]


    listas_valores = []

    #print(f'{nombres_params}')


    # para cada termino del diccionario
    # extraemos sus valores de incio, fin y partcion
    # y con esos creamos un linspace para cada uno
    for nom in nombres_params:
        ini, fin, pasos = config_barrido[nom]
        listas_valores.append(np.linspace(ini, fin, pasos + 1))
    #  Ej ==> Para EA me va a dar 0.2, 0.4, 0.6, 0.8 ,1
    #         Para TBA me va a dar 0, 1
    # A estos valores les hace append en el array de valores

    #print(f'final: {listas_valores}')


    # La libreria de itertools, tiene muchas funciones
    # para ciclos, iteraciones, conteos, etc
    # es para evitar los 8 for anidados de variar cada
    # parametro "a mano"

    # .product:
    # Esta funcion da el producto cartesiano de la lista
    # que le pongas

    # En este caso nos va a generr todas las combinaciones
    # param que estamos variando

    combinaciones = list(itertools.product(*listas_valores))
    # Ej ==> Da todas las comb de [0.0, 0.2, 0.4, 0.6, 0.8 ,1] y [0, 1]
    #        para el caso del ejemplo son 12 iteraciones
    #        nos va a dar una lista de la forma
    #        [(0.0, 0) , (0.2, 0) , ... , (0.8, 1) , (1, 1)]

    #print(f'Array de combinaciones: {combinaciones}')
    print(f"Total de simulaciones a realizar: {len(combinaciones)}")


    for comb in combinaciones:
        # Creamos otro diccionario que relaciona
        # cada parametro con su valor en la combinacion correspondiente
        #  {Nombres: Valores}
        dict_nuevo = dict(zip(nombres_params, comb))

        # Esta seccion genera el nombre del arhcivo

        # Sustituir los puntos "." por otra cosa para que no de
        # problemas al leer los archivos
        nombre = "_".join([f"{param}{valor:.1f}" for param, valor in dict_nuevo.items()]).replace(".", "p")

        # Para el nombre final especificamos que es de la cadena original
        nombre_out = f"trans_o_{nombre}.dat"


        #print(f"Simulando la combinacion: {dict_nuevo} ---")

        ### Seccion de modificacion del archivo original
        #   de fortran
        with open(archivo_base, 'r') as f:
            lineas = f.readlines()

        with open(nombre_temp, 'w') as f:

            for linea in lineas:
                # A cada linea le ponemos un marcador
                # para saber si ya fue modificada o no
                # o en su defecto si No debe ser modificada
                modificacion = False

                for p_nom, p_val in dict_nuevo.items():
                  # Si la linea contiene alguino de los param
                  # que queremos modificar, los reescribimos
                    if f"{p_nom} =" in linea:
                        ### TENEMOS QUE CONSIDERAR EL CASO DONDE EDITAMOS N_final

                        # Si el param a variar es  N_final
                        # debe ser un entero

                        # para el resto de casos es un REAL(16)
                        # esos van con q0
                        val_str = f"{int(p_val)}" if p_nom == "N_final" else f"{p_val:.4f}q0"
                        f.write(f"      {p_nom} = {val_str} ! Editado Automaticamnte\n")

                        # En caso de hacer lo anterior, cambiamos el
                        # marcador de modifacion a verdadero
                        modificacion = True
                        break
                # Una vez revisado con el for
                # ahora revisamos con el marcador de modifcacion
                # si no estan modificadas las dejamos igual

                # A menos que sea la linea del OPEN, donde se crea el archivo
                # de guardado
                if not modificacion:
                    if "OPEN(1, FILE=" in linea:
                        f.write(f"      OPEN(1, FILE='{nombre_out}')\n")
                    else:
                        f.write(linea)

        # Compilar con todos los extras que necesita el gfortran
        compile_cmd = f'gfortran -cpp -DQCMPLX\(a,b\)="cmplx(a,b,16)" -fno-range-check {nombre_temp} -o ejecutable'

        # Ejecutar el codigo
        if os.system(compile_cmd) == 0:
            if os.system("./ejecutable") == 0:
                
                grafica_especial(nombre_out, dict_nuevo)
            else:
                print(f"Error en ejecucion para {dict_nuevo}")
        else:
            print(f"Error en compilacion para {dict_nuevo}")
    #
    if os.path.exists(nombre_temp): os.remove(nombre_temp)
    print("\n FINALIZADO")
# ---------------------------------------------------------
#                         PRUEBAS
# ---------------------------------------------------------
# diccionario:
# {param_1: (inicio, fin, particion), param_2(i,f,p) , etc}
# El nombre del parametro debe ser IDENTICO al nombre en el fortran

test = {"EA": (0, 1, 5), "TBA": (-1, 0, 1)}

test_1 = {"EA": (0, 0, 0), "EB": (-1, 1, 10)}

test_2 = {
    "EA": (0.0, 0.5, 1),
    "EB": (0.0, 0.5, 1),
    "TBA": (-0.8, -0.4, 1),
}

#variacion_muiltitple(test)
variacion_muiltitple(test_1)
#variacion_multiple(test_2)

# YA funciona, el test 2 es el que hice con los for anidados y coinciden