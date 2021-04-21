package com.example.uart_android_test

import android.content.Context
import android.hardware.usb.UsbDeviceConnection
import android.hardware.usb.UsbManager
import android.os.Handler
import android.os.Looper
import com.example.uart_android_test.databinding.ActivityMainBinding
import com.hoho.android.usbserial.driver.UsbSerialDriver
import com.hoho.android.usbserial.driver.UsbSerialPort
import com.hoho.android.usbserial.driver.UsbSerialProber
import com.hoho.android.usbserial.util.SerialInputOutputManager
import java.util.concurrent.Executors


public class UsbSerialAS7265x: SerialInputOutputManager.Listener {

    private lateinit var binding: ActivityMainBinding
    private lateinit var driver: UsbSerialDriver
    private lateinit var connection: UsbDeviceConnection
    private lateinit var port: UsbSerialPort

    private lateinit var usbIoManager: SerialInputOutputManager
    var mainLooper: Handler = Handler(Looper.getMainLooper())

    private val WRITE_WAIT_MILLIS = 2000
    private val READ_WAIT_MILLIS = 2000

    val ATLED0_1 = byteArrayOf(0x41, 0x54, 0x4C, 0x45, 0x44, 0x30, 0x3D, 0x31, 0x0D, 0x0A)
    val ATLED0_0 = byteArrayOf(0x41, 0x54, 0x4C, 0x45, 0x44, 0x30, 0x3D, 0x30, 0x0D, 0x0A)
    val ATCDATA = byteArrayOf(0x41, 0x54, 0x43, 0x44, 0x41, 0x54, 0x41, 0x0D, 0x0A)
    val ATDATA = byteArrayOf(0x41, 0x54, 0x44, 0x41, 0x54, 0x41, 0x0D, 0x0A)

    enum class EstadoComunicacao{
        AGUARDANDO, RECEBENDO, RECEBIDO
    }

    var estadoCom: EstadoComunicacao = EstadoComunicacao.AGUARDANDO
    var dadosSerial: String = ""

    fun connect(context: Context){

        var manager: UsbManager = context.getSystemService(Context.USB_SERVICE) as UsbManager
        var availableDrivers = UsbSerialProber.getDefaultProber().findAllDrivers(manager)


        if (availableDrivers.isEmpty()) {
            status("connection failed: device not found");
            return;
        }
        else {

            try {
                driver = availableDrivers[0]

                connection = manager.openDevice(driver.device)

                port = driver.ports[0]
                port.open(connection)

                port.setParameters(115200, 8, UsbSerialPort.STOPBITS_1, UsbSerialPort.PARITY_NONE)

                usbIoManager = SerialInputOutputManager(port, this)
                Executors.newSingleThreadExecutor().submit(usbIoManager)
                status("connected")
            }
            catch (e: Exception) {
                status("connection failed: " + e.message);
                disconnect()
            }

        }
    }

    fun disconnect() {
        if (usbIoManager != null) usbIoManager!!.stop()
        try {
            port.close();
        }
        catch (e: Exception) {}
        status("disconnected")
    }

    private fun status(s: String) {

    }

    fun send(data: ByteArray){
        port.write(data, WRITE_WAIT_MILLIS)
    }

    private fun receive(data: ByteArray) {

        var len: Int
        var dadosRecebidos: List<String>
        var qtdDados: Int = 0
        var retorno: String

        if ((estadoCom == EstadoComunicacao.AGUARDANDO) || (estadoCom == EstadoComunicacao.RECEBIDO)) {
            estadoCom = EstadoComunicacao.RECEBENDO

            retorno = String(data)
            dadosSerial = retorno


            dadosRecebidos = dadosSerial.split("OK")
            qtdDados = dadosRecebidos.size

            if(qtdDados>1) {
                //variavel de estado aqui
                estadoCom = EstadoComunicacao.RECEBIDO
            }
        }
        else if (estadoCom == EstadoComunicacao.RECEBENDO) {

            retorno = String(data)
            dadosSerial += retorno

            dadosRecebidos = dadosSerial.split("OK")
            qtdDados = dadosRecebidos.size

            if(qtdDados>1) {
                //variavel de estado aqui
                estadoCom = EstadoComunicacao.RECEBIDO
            }
        }
    }

    override fun onRunError(e: Exception) {
        mainLooper.post {
            status("connection lost: " + e.message);
            port.close()
        }
    }

    override fun onNewData(data: ByteArray) {
        mainLooper.post { receive(data) }
    }

}