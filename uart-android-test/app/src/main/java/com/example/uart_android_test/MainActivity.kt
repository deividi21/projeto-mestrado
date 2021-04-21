package com.example.uart_android_test

import android.os.Bundle
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import com.example.uart_android_test.databinding.ActivityMainBinding
import com.example.uart_android_test.UsbSerialAS7265x


class MainActivity : AppCompatActivity(){

    lateinit var AS7265x: UsbSerialAS7265x

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        var binding = ActivityMainBinding.inflate(layoutInflater)
        setContentView(binding.root)

        var botaoConectar = binding.btConectar
        var botaoDesconectar = binding.btDesconectar
        var botaoLigarLed = binding.btLigarLed
        var botaoDesligarLed = binding.btDesligarLed
        var botaoColetarDados = binding.btColetarDados
        var botaoColetarDadosCalibrados = binding.btColetarDadosCalibrados
        var botaoColetarRetorno = binding.btColetarRetorno


        botaoConectar.setOnClickListener{
            AS7265x.connect(this)
        }

        botaoDesconectar.setOnClickListener{
            AS7265x.disconnect()
        }

        botaoLigarLed.setOnClickListener{

        }

        botaoDesligarLed.setOnClickListener{

        }

        botaoColetarDadosCalibrados.setOnClickListener{

        }

        botaoColetarDados.setOnClickListener{

        }
    }

}

