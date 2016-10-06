
CREATE TABLE `role` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '角色id',
  `account_name` varchar(100) NOT NULL DEFAULT '' COMMENT '账号',
  `password` varchar(100) NOT NULL DEFAULT '' COMMENT '密码',
  PRIMARY KEY (`id`,`account_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='角色信息';

