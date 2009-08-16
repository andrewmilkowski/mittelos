package com.github.psnively.mittelos

import System._
import java.lang.reflect.{Field, Method, Modifier}
import scala.collection.mutable.ArrayBuffer

object mittelos
{
  System.loadLibrary(get_library_name("minisat"))

  object OSTypeEnum
  {
	private var list = new ArrayBuffer[OSType] //temporary list
    class OSType()
    {
      list += this
      override def toString = (OSTypeEnum.getClass.getDeclaredMethods.find((a: Method) => {Modifier.isPublic(a.getModifiers) && a.invoke(OSTypeEnum.this) == this})).get.getName
    }

    val APPLE, LINUX, SUN, UNKNOWN, WINDOWS = new OSType
    val values = list.toList
  }

  object ARCHTypeEnum
  {
	private var list = new ArrayBuffer[ARCHType] //temporary list
    class ARCHType()
    {
      list += this
      override def toString = (ARCHTypeEnum.getClass.getDeclaredMethods.find((a: Method) => {Modifier.isPublic(a.getModifiers) && a.invoke(ARCHTypeEnum.this) == this})).get.getName
    }

    val PPC, PPC_64, SPARC, UNKNOWN, X86, X86_64 = new ARCHType
    val values = list.toList
  }

  private def get_library_name(name: String): String =
  {
	val os_name_string = System.getProperty("os.name").toLowerCase()
	val os_arch_string = System.getProperty("os.arch").toLowerCase()

	var os_type: OSTypeEnum.OSType = OSTypeEnum.APPLE
	var os_arch: ARCHTypeEnum.ARCHType = ARCHTypeEnum.PPC

	var result = name + "_"

	if (os_name_string.startsWith("mac os x"))
	{
	  os_type = OSTypeEnum.APPLE
	}
	else if (os_name_string.startsWith("windows"))
	{
	  os_type = OSTypeEnum.WINDOWS
	}
	else if (os_name_string.startsWith("linux"))
	{
	  os_type = OSTypeEnum.LINUX
	}
	else if (os_name_string.startsWith("sun"))
	{
	  os_type = OSTypeEnum.SUN
	}
	else os_type = OSTypeEnum.UNKNOWN

	if (os_arch_string.equals("i386"))
	{
      os_arch = ARCHTypeEnum.X86
    }
	else if (os_arch_string.startsWith("amd64") || os_arch_string.startsWith("x86_64"))
    {
      os_arch = ARCHTypeEnum.X86_64
    }
	else if (os_arch_string.equals("ppc"))
    {
      os_arch = ARCHTypeEnum.PPC
    }
	else if (os_arch_string.startsWith("ppc"))
    {
      os_arch = ARCHTypeEnum.PPC_64
    }
	else if (os_arch_string.startsWith("sparc"))
    {
      os_arch = ARCHTypeEnum.SPARC
    }
    else os_arch = ARCHTypeEnum.UNKNOWN

    result = result + os_type + "_" + os_arch
    os_type match
    {
    	case OSTypeEnum.APPLE => result = "lib" + result + ".jnilib"
    	case OSTypeEnum.WINDOWS => result = result + ".dll"
    	case _ => result = "lib" + result + ".so"
    }

    result
  }

  def main(args: Array[String]) =
  {
	  println("Hello, whirled!")
  }
}
