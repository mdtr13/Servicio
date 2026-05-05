import matplotlib.pyplot as plt
import numpy as np

# Nombre del archivo con los datos
archivo = 'trans_o_g11.dat'

try:
    data = np.loadtxt(archivo)
    x = data[:, 0] # Primera columna (E)
    y = data[:, 1] # Segunda columna (Transmitancia)

    plt.figure(figsize=(10, 6))
    plt.plot(x, y, label='Transmitancia', color='blue', linewidth=1.5)

    plt.title('Transmitancia cadena Original, n=11', fontsize=14)
    plt.xlabel('Energia (E)', fontsize=12)
    plt.ylabel('Transmitancia', fontsize=12)
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.legend()

    plt.show()

except Exception as e:
    print(f"Error al leer el archivo: {e}")