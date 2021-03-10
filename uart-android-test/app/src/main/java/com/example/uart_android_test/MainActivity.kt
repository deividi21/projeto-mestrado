package com.example.uart_android_test

import android.hardware.usb.UsbDeviceConnection
import android.hardware.usb.UsbManager
import android.os.Bundle
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import com.example.uart_android_test.databinding.ActivityMainBinding
import com.hoho.android.usbserial.driver.UsbSerialDriver
import com.hoho.android.usbserial.driver.UsbSerialPort
import com.hoho.android.usbserial.driver.UsbSerialProber


class MainActivity : AppCompatActivity() {

    lateinit var binding: ActivityMainBinding
    lateinit var driver: UsbSerialDriver
    lateinit var connection: UsbDeviceConnection
    lateinit var port: UsbSerialPort

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityMainBinding.inflate(layoutInflater)

        setContentView(binding.root)

        var botaoConectar = binding.btConectar
        var botaoDesconectar = binding.btDesconectar
        var botaoLigarLed = binding.btLigarLed
        var botaoDesligarLed = binding.btDesligarLed

        botaoConectar.setOnClickListener{

            var manager: UsbManager = getSystemService(USB_SERVICE) as UsbManager
            var availableDrivers = UsbSerialProber.getDefaultProber().findAllDrivers(manager)


            if (availableDrivers.isEmpty()) {
                Toast.makeText(
                    applicationContext,
                    "Nenhum dispositivo encontrado",
                    Toast.LENGTH_SHORT
                ).show()

            }
            else {

                driver = availableDrivers[0]

                connection = manager.openDevice(driver.device)

                port = driver.ports[0]
                port.open(connection)
                port.setParameters(115200, 8, UsbSerialPort.STOPBITS_1, UsbSerialPort.PARITY_NONE)

            }
        }

        botaoLigarLed.setOnClickListener{
            val ligaLed = byteArrayOf(0x41,0x54,0x4C,0x45,0x44,0x30,0x3D,0x31,0x0D,0x0A)

            port.write(ligaLed, 0)

            Toast.makeText(applicationContext,ligaLed.toString(),Toast.LENGTH_SHORT).show()

        }

        botaoDesligarLed.setOnClickListener{
            val desligaLed = byteArrayOf(0x41,0x54,0x4C,0x45,0x44,0x30,0x3D,0x30,0x0D,0x0A)

            port.write(desligaLed, 0)
            Toast.makeText(applicationContext,"ATLEDC=0".toByteArray().toString(),Toast.LENGTH_SHORT).show()
        }
        botaoDesconectar.setOnClickListener{
            port.close()
        }

    }
}