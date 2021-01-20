package com.realtime.common

import org.apache.commons.lang.StringUtils

import java.text.SimpleDateFormat
import java.util.{Calendar, Locale}

/**
 *@Description: 非组件的公共方法
 */
object CommonTool {

  // TODO ============================================ udf函数 ============================================
  // 过滤表情
  def getSubsidy(str: String): String = {
    var out = new StringBuffer()
    if (str == null || ("".equals(str))){
      ""
    }else{
      var chars = str.toCharArray()
      for(i <- 0 until chars.length) {
        if(
          (chars(i) >= 19968 && chars(i) <= 40869) //中日朝兼容形式的unicode编码范围： U+4E00——U+9FA5
            || (chars(i) >= 11904 && chars(i) <= 42191)//中日朝兼容形式扩展
            || (chars(i) >= 63744 && chars(i) <= 64255)//中日朝兼容形式扩展
            || (chars(i) >= 65072 && chars(i) <= 65103)//中日朝兼容形式扩展
            || (chars(i) >= 65280 && chars(i) <= 65519)//全角ASCII、全角中英文标点、半宽片假名、半宽平假名、半宽韩文字母的unicode编码范围：U+FF00——U+FFEF
            || (chars(i) >= 32 && chars(i) <= 126)//半角字符的unicode编码范围：U+0020-U+007e
            || (chars(i) >= 12289 && chars(i) <= 12319)//全角字符的unicode编码范围：U+3000——U+301F
        ) {
          out.append(chars(i))
        }
      }
      out.toString
    }
  }

  // 删除表情
  def removeEmoji(str: String): String = {
    if (StringUtils.isBlank(str)) {
      ""
    } else {
      val characterFilter = "[^\\p{L}\\p{M}\\p{N}\\p{P}\\p{Z}\\p{Cf}\\p{Cs}\\s]"
      str.replaceAll(characterFilter, "")
    }
  }


  // TODO ============================================ 代码方法 能用sql实现 ============================================
  /**
   * 获取星期几 返回string
   * @param dateStr
   * @return String
   */
  def getDayWeek(dateStr: String) = {
    var dayWeek: String = null
    if (StringUtils.isNotBlank(dateStr)) {
      val formatter: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd", Locale.US)
      val cal: Calendar = Calendar.getInstance()
      val dt = formatter.parse(dateStr)
      var weekDays = Array("星期日", "星期一", "星期二", "星期三", "星期四", "星期五", "星期六")
      cal.setTime(dt)
      var w = cal.get(Calendar.DAY_OF_WEEK) - 1
      if (w < 0) {
        w = 0
      }
      dayWeek = weekDays(w)
    }
    dayWeek
  }




}
