import os
import matplotlib.pyplot as plt
import numpy as np
import itertools

def variacion_multiple(config_barrido, archivo_base="transmitancia_original.f"):
    """
    config_barrido: Diccionario que configura
                    como se van a variar los parametros
                    del fortran
                    {}"parametro_1":(inicio, fin, pasos) , "para_2":(inicio, fin, pasos) , etc}

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
    print(f"Cantidad de simulaciones: {len(combinaciones)}")


    for comb in combinaciones:
        # Creamos otro diccionario que relaciona
        # cada parametro con su valor en la combinacion correspondiente
        #  {Nombres: Valores}
        dict_nuevo = dict(zip(nombres_params, comb))

        # Esta seccion genera el nombre del arhcivo

        # Sustituir los puntos "." por otra cosa para que no de
        # problemas al leer los archivos

        ########################################################################
        ### PROBLEMA ###

        # al darle nombre a los archivos, si editamos muchos parametros de
        # manera simultanea el nombre se vuelve muy largo y da error al compilar

        #nombre = "_".join([f"{param}{valor:.1f}" for param, valor in dict_nuevo.items()]).replace(".", "p")
        nombre = "TEST"

        # Para el nombre final especificamos que es de la cadena original
        nombre_out = f"trans_o_{nombre}.dat"
        ########################################################################

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
                # Un error importante pasa cuando uno de los
                # parametros a variar es E, ya que en el codigo de
                # fortran "E =" es una linea que aparece mas de una
                # vez durante algunos calculos, no es un parametro
                # 100% fijo ya que justo el DO del barrido lo modifica
                # en cada iteracion

                # Por eso agregamos una condicion que modifique
                # solo la linea que explicitamente define la E inical

                if 'E = 0.0q0' in linea and 'E' in dict_nuevo:
                  val_str = f"{p_val:.4f}q0"
                  f.write(f"      E = {val_str} ! Editado Automaticamnte\n")
                  modificacion = True

                else: #Para el resto de parametros hacemos lo que hacia el codigo anteriro
                  #a
                  for p_nom, p_val in dict_nuevo.items():

                    if p_nom == "E": continue # Saltamos E porque el caso E ya lo quitamos

                    a_cambiar = f"      {p_nom} ="

                    if a_cambiar in linea:
                      #a

                      # Si la linea contiene alguino de los param
                      # que queremos modificar, los reescribimos

                      ### TENEMOS QUE CONSIDERAR EL CASO DONDE EDITAMOS N_final

                      # Si el param a variar es  N_final
                      # debe ser un entero

                      # para el resto de casos es un REAL(16)
                      # esos van con q0
                      if p_nom == "N_final":
                        val_str = f"{int(p_val)}"
                      else:
                        val_str = f"{p_val:.4f}q0"

                      f.write(f"      {p_nom} = {val_str} ! Editado Automaticamnte\n")
                      modificacion = True
                      break
                        # En caso de hacer lo anterior, cambiamos el
                        # marcador de modifacion a verdadero

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

                graficar_con_param(nombre_out, dict_nuevo)
            else:
                print(f"Error en ejecución para {dict_nuevo}")
        else:
            print(f"Error en compilación para {dict_nuevo}")
    #
    if os.path.exists(nombre_temp): os.remove(nombre_temp)
    print("\n FINALIZADO")

def graficar_con_param(archivo_dat, dic_params):
    try:

        data = np.loadtxt(archivo_dat)
        plt.figure(figsize=(10, 6))

        # Meter un cuadro a la derecha con la lista de params
        # Espacio para el cuadro a la derecha
        plt.subplots_adjust(right=0.7)

        # El plot es el regular para los datos generados
        # que se leen de un .dat
        plt.plot(data[:, 0], data[:, 1], color='red', lw=1)
        plt.title(f"Transmitancia: {archivo_dat}", fontsize=10)
        plt.grid(True, alpha=0.3)

        # Texto de los parametros
        info = "Paametros usados:\n" + "\n\n".join([f"{k}: {v:.2f}" for k, v in dic_params.items()])
        plt.text(1.05, 0.95, info, transform=plt.gca().transAxes, fontsize=9,
                 verticalalignment='top', bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))

        #plt.savefig(archivo_dat.replace('.dat', '.png'))
        plt.show()
        plt.close()

        # Estaria bueno, poder senalar con un color
        # cual es el parametro que cambio respecto de
        # la graf anterior
    except:
        print(f"No se pudo graficar {archivo_dat}")



# ---------------------------------------------------------
#                         PRUEBAS
# ---------------------------------------------------------
# diccionario:
# {param_1: (inicio, fin, particion), param_2(i,f,p) , etc}
# El nombre del parametro debe ser IDENTICO al nombre en el fortran

prueba = {"EA": (0, 1, 5), "TBA": (-1, 0, 1)}

prueba_1 = {"EA": (0, 0, 0), "EB": (-1, 1, 10)}

prueba_2 = {
    "EA": (0.0, 0.5, 1),
    "EB": (0.0, 0.5, 1),
    "N_final": (5, 5, 0),
}

#variacion_multiple(prueba)
#variacion_multiple(prueba_1)
#variacion_multiple(prueba_2)

caso_base = {
    "E" : (0.0, 0.0, 0),
    "EA": (0.0, 0.0, 0),
    "EB": (0.0, 0.0, 0),
    "TAA": (-1.0, -1.0, 0),
    "TAB": (-0.8, -0.8, 0),
    "TBA": (-0.8, -0.8, 0),
    "N_final": (10, 10, 0),
    "T": (-1, -1, 0),
}

vacio = {}

test = {
    "E" : (0.0, 0.0, 0),
    "EA": (-1.0, 1.0, 1),
    "EB": (-1.0, 1.0, 1),
    "TAA": (-1.0, -1.0, 0),
    "TAB": (-1.0, -1.0, 0),
    "TBA": (-1.0, -1.0, 0),
    "N_final": (7, 10, 3),
    "T": (-2, -2, 1),
}

#variacion_multiple(vacio)
#variacion_multiple(caso_base)

variacion_multiple(test)