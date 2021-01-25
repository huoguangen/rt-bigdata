package com.realtime.common

import org.apache.commons.lang.StringUtils

/**
*@Description: 业务规则 要是有多个不同平台 可再区分多一个类
*/
object BusinessRulesTool {

  // TODO ============================================ udf函数 ============================================
  // 根据评论内容得到评论类型
  def getCommentType(content: String): String = {
    import util.control.Breaks._
    var externalClass = ""
    val deliveryProblem = "超时-慢-配送时间-长-快-速-准-迟-晚-凉-久 "
    val productProblem = "半杯-一半-少-三分-分量-量少-不足-淡-焦-口味-难吃-稀释-胃口-不合-味道-口感-温度-熟-咸-油-辣-不好吃-冰的-不满意-太硬-糊了-不热-不好喝-酸-过期-变质-沉淀物-虫-身体-头发-卫生-蚊虫-异物-蜘蛛-质量-体毛-睫毛-品质-肚子疼-拉肚子-石头-石子-塑料-抹布-钢丝球-纸-玻璃-蟑螂-好吃-不错-挺好-难吃-足-饱-不够-不饱-不对-不是-冰-冷-不一致-不同-不一样-和之前收到餐品不符"
    val serviceProblem = "态度-恶劣-服务-语气-强硬-骂人-脏话-拒接-吵-打-推"
    val riderProblem = "没收到-没有收到-车祸-超商圈-超区-超范围-配送范围-超出商圈-骑手电话-骑手联系-距离远-取错餐-小哥-送餐-骑手-地址-地方-拿错"
    val storeProblem = "包装-残渣-倒-翻-封口-盖-烂-裂-漏-泼-破-洒-撒-湿-损-溢出-打烊-卖光-卖完-没货-没有货-没有了-缺货-商品没有-无货-售罄-估清-关-关店-没开门-无法配送-售完-沽清-售尽-错送-没配送-少送-送成了-送错-送来的是-吸管-筷子-餐具-纸巾-勺子-去冰-做错-门店-餐厅-店-不看备注"

    if (StringUtils.isNotBlank(content)) {
      breakable(
        for (delivery <- deliveryProblem.split("-")) {
          if (content.contains(delivery)) {
            externalClass = "送餐时间"
            break()
          }
        })
      breakable(
        for (product <- productProblem.split("-")) {
          if (content.contains(product)) {
            if (externalClass != "") externalClass += ",餐品方面"

            else
              externalClass = "餐品方面"
            break()
          }
        })
      breakable(
        for (service <- serviceProblem.split("-")) {
          if (content.contains(service)) {
            if (externalClass != "") externalClass += ",服务方面"
            else externalClass = "服务方面"
            break()
          }
        })
      breakable(
      for (rider <- riderProblem.split("-")) {
        if (content.contains(rider)) {
          if (externalClass != "") externalClass += ",骑手方面"
          else externalClass = "骑手方面"
        }
      })
      breakable(
        for (store <- storeProblem.split("-")) {
          if (content.contains(store)) {
            if (externalClass != "") externalClass += ",餐厅方面"
            else externalClass = "餐厅方面"
            break()
          }
        })
      if (!externalClass.contains("方面") && !externalClass.contains("送餐")) externalClass = "其他"
    }
    externalClass
  }



}
