package com.example.uart_android_test

import android.hardware.usb.UsbDeviceConnection
import android.hardware.usb.UsbManager
import android.os.Bundle
import android.text.SpannableStringBuilder
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import com.example.uart_android_test.databinding.ActivityMainBinding
import com.hoho.android.usbserial.driver.UsbSerialDriver
import com.hoho.android.usbserial.driver.UsbSerialPort
import com.hoho.android.usbserial.driver.UsbSerialProber
import com.hoho.android.usbserial.util.SerialInputOutputManager
import java.util.concurrent.Executors


class MainActivity : AppCompatActivity() {

    lateinit var binding: ActivityMainBinding
    lateinit var driver: UsbSerialDriver
    lateinit var connection: UsbDeviceConnection
    lateinit var port: UsbSerialPort

    private var usbIoManager: SerialInputOutputManager? = null

    private val WRITE_WAIT_MILLIS = 2000
    private val READ_WAIT_MILLIS = 2000

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)

        var botaoConectar = binding.btConectar
        var botaoDesconectar = binding.btDesconectar
        var botaoLigarLed = binding.btLigarLed
        var botaoDesligarLed = binding.btDesligarLed
        var botaoColetarDados = binding.btColetarDados
        var botaoColetarDadosCalibrados = binding.btColetarDadosCalibrados
        var botaoColetarRetorno = binding.btColetarRetorno

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

                usbIoManager = SerialInputOutputManager(port)
                Executors.newSingleThreadExecutor().submit(usbIoManager)

                port.setParameters(115200, 8, UsbSerialPort.STOPBITS_1, UsbSerialPort.PARITY_NONE)

            }
        }

        botaoDesconectar.setOnClickListener{
            if (usbIoManager != null) usbIoManager!!.stop()
            usbIoManager = null
            port.close()
        }

        botaoLigarLed.setOnClickListener{
            val ligaLed = byteArrayOf(0x41, 0x54, 0x4C, 0x45, 0x44, 0x30, 0x3D, 0x31, 0x0D, 0x0A)
            //                              A      T     L     E     D     0     =     1    \r    \n

            port.write(ligaLed, WRITE_WAIT_MILLIS)
        }

        botaoDesligarLed.setOnClickListener{
            val desligaLed = byteArrayOf(0x41, 0x54, 0x4C, 0x45, 0x44, 0x30, 0x3D, 0x30, 0x0D, 0x0A)
            //                              A      T     L     E     D     0     =     0    \r    \n

            port.write(desligaLed, WRITE_WAIT_MILLIS)
        }

        botaoColetarDadosCalibrados.setOnClickListener{
            val coletarDadosCalibrados = byteArrayOf(0x41,
                0x54,
                0x43,
                0x44,
                0x41,
                0x54,
                0x41,
                0x0D,
                0x0A)
            //                                        A      T     C     D     A     T     A    \r    \n

            port.write(coletarDadosCalibrados, WRITE_WAIT_MILLIS)
        }

        botaoColetarDados.setOnClickListener{
            val coletarDados = byteArrayOf(0x41, 0x54, 0x44, 0x41, 0x54, 0x41, 0x0D, 0x0A)
            //                               A      T    D     A     T     A    \r    \n

            port.write(coletarDados, WRITE_WAIT_MILLIS)
        }


        fun onNewData(data: ByteArray?) {

        }

        open fun receive(data: ByteArray) {
            val spn = SpannableStringBuilder()
            spn.append("""receive ${data.size} bytes
""")
            if (data.size > 0) spn.append(HexDump.dumpHexString(data).toString() + "\n")
            receiveText.append(spn)
        }
    }



    private fun receberDadosSerial(){

        var textoRetornoSerial = binding.textViewRetornoSerial
        var len: Int
        var buffer = ByteArray(1024)
        var finalizador: Boolean = false
        var retorno: String
        var qtdDados: Int = 0
        var dadosRecebidos: List<String>
        var dados: List<Float>


        while(!finalizador)
        {
            len = port.read(buffer, READ_WAIT_MILLIS);
            retorno = String(buffer)

            dadosRecebidos = retorno.split("OK")
            qtdDados = dadosRecebidos.size

            while (qtdDados <= 1)
            {
                len = port.read(buffer, READ_WAIT_MILLIS);
                retorno = String(buffer)

                dadosRecebidos = retorno.split("OK")
                qtdDados = dadosRecebidos.size
            }

            for (c in 0 until qtdDados)
            {
                val toast = Toast.makeText(applicationContext,
                    qtdDados.toString(),
                    Toast.LENGTH_SHORT).show()
            }

            textoRetornoSerial.text = retorno

            finalizador = true
        }


    }

    private fun limparSerial(){
        var len: Int
        var buffer = ByteArray(1024)
        len = port.read(buffer, READ_WAIT_MILLIS)
        val toast = Toast.makeText(applicationContext, String(buffer), Toast.LENGTH_SHORT).show()
    }
}

