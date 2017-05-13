#im_rf_cls_group
########################
set.seed(1)
model_rf_cls_group90 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                       v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21+시간비율_인터넷.컴퓨터+cat_coef_time+s_prt_sd+소모시간_뉴스.미디어+시간비율_비즈니스.경제+
                                       횟수비율_비즈니스.경제+횟수_교육.학원+시간비율_커뮤니티+횟수비율_엔터테인먼트+시간비율_서비스+day_coef_cnt+횟수비율_커뮤니티+day_coef_visit+mon_coef_visit+s_t_max+
                                       p_v_.16+횟수_서비스+v_t_sd+p_v_.18+p_v_.2+소모시간_생활.가정.취미+p_v_.10+mon_coef_cnt+횟수비율_정보통신.IT+소모시간_쇼핑+
                                       횟수_생활.가정.취미+횟수_제조+횟수비율_교육.학원+시간비율_엔터테인먼트+횟수비율_서비스+소모시간_서비스+소모시간_금융.부동산+cat_coef_cnt+s_pr_max+횟수_커뮤니티+
                                       wednes_z2+p_v_.15+시간비율_정보통신.IT+wednes_ratio2+소모시간_교육.학원+s_v_max+n_v_.5+p_v_.8+시간비율_교육.학원+net_day,
                                     data = cs_merge_train, 
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group90

set.seed(1)
model_rf_cls_group85 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                       v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21+시간비율_인터넷.컴퓨터+cat_coef_time+s_prt_sd+소모시간_뉴스.미디어+시간비율_비즈니스.경제+
                                       횟수비율_비즈니스.경제+횟수_교육.학원+시간비율_커뮤니티+횟수비율_엔터테인먼트+시간비율_서비스+day_coef_cnt+횟수비율_커뮤니티+day_coef_visit+mon_coef_visit+s_t_max+
                                       p_v_.16+횟수_서비스+v_t_sd+p_v_.18+p_v_.2+소모시간_생활.가정.취미+p_v_.10+mon_coef_cnt+횟수비율_정보통신.IT+소모시간_쇼핑+
                                       횟수_생활.가정.취미+횟수_제조+횟수비율_교육.학원+시간비율_엔터테인먼트+횟수비율_서비스+소모시간_서비스+소모시간_금융.부동산+cat_coef_cnt+s_pr_max+횟수_커뮤니티+
                                       wednes_z2+p_v_.15+시간비율_정보통신.IT+wednes_ratio2+소모시간_교육.학원,
                                     data = cs_merge_train, 
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group85

set.seed(1)
model_rf_cls_group80 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                       v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21+시간비율_인터넷.컴퓨터+cat_coef_time+s_prt_sd+소모시간_뉴스.미디어+시간비율_비즈니스.경제+횟수비율_비즈니스.경제+
                                       횟수_교육.학원+시간비율_커뮤니티+횟수비율_엔터테인먼트+시간비율_서비스+day_coef_cnt+횟수비율_커뮤니티+day_coef_visit+mon_coef_visit+s_t_max+
                                       p_v_.16+횟수_서비스+v_t_sd+p_v_.18+p_v_.2+소모시간_생활.가정.취미+p_v_.10+mon_coef_cnt+횟수비율_정보통신.IT+소모시간_쇼핑+
                                       횟수_생활.가정.취미+횟수_제조+횟수비율_교육.학원+시간비율_엔터테인먼트+횟수비율_서비스+소모시간_서비스+소모시간_금융.부동산+cat_coef_cnt+s_pr_max+횟수_커뮤니티,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group80

set.seed(1)
model_rf_cls_group75 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                       v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21+시간비율_인터넷.컴퓨터+cat_coef_time+s_prt_sd+소모시간_뉴스.미디어+시간비율_비즈니스.경제+횟수비율_비즈니스.경제+
                                       횟수_교육.학원+시간비율_커뮤니티+횟수비율_엔터테인먼트+시간비율_서비스+day_coef_cnt+횟수비율_커뮤니티+day_coef_visit+mon_coef_visit+s_t_max+
                                       p_v_.16+횟수_서비스+v_t_sd+p_v_.18+p_v_.2+소모시간_생활.가정.취미+p_v_.10+mon_coef_cnt+횟수비율_정보통신.IT+소모시간_쇼핑+
                                       횟수_생활.가정.취미+횟수_제조+횟수비율_교육.학원+시간비율_엔터테인먼트+횟수비율_서비스,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group75

set.seed(1)
model_rf_cls_group70 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                       v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21+시간비율_인터넷.컴퓨터+cat_coef_time+s_prt_sd+소모시간_뉴스.미디어+시간비율_비즈니스.경제+
                                       횟수비율_비즈니스.경제+횟수_교육.학원+시간비율_커뮤니티+횟수비율_엔터테인먼트+시간비율_서비스+day_coef_cnt+횟수비율_커뮤니티+day_coef_visit+mon_coef_visit+s_t_max+
                                       p_v_.16+횟수_서비스+v_t_sd+p_v_.18+p_v_.2+소모시간_생활.가정.취미+p_v_.10+mon_coef_cnt+횟수비율_정보통신.IT+소모시간_쇼핑,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group70

set.seed(1)
model_rf_cls_group65 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                       v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21+시간비율_인터넷.컴퓨터+cat_coef_time+s_prt_sd+소모시간_뉴스.미디어+시간비율_비즈니스.경제+
                                       횟수비율_비즈니스.경제+횟수_교육.학원+시간비율_커뮤니티+횟수비율_엔터테인먼트+시간비율_서비스+day_coef_cnt+횟수비율_커뮤니티+day_coef_visit+mon_coef_visit+s_t_max+
                                       p_v_.16+횟수_서비스+v_t_sd+p_v_.18+p_v_.2,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group65

set.seed(1)
model_rf_cls_group60 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                       v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21+시간비율_인터넷.컴퓨터+cat_coef_time+s_prt_sd+소모시간_뉴스.미디어+시간비율_비즈니스.경제+
                                       횟수비율_비즈니스.경제+횟수_교육.학원+시간비율_커뮤니티+횟수비율_엔터테인먼트+시간비율_서비스+day_coef_cnt+횟수비율_커뮤니티+day_coef_visit+mon_coef_visit+s_t_max,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group60

set.seed(1)
model_rf_cls_group55 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                       v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21+시간비율_인터넷.컴퓨터+cat_coef_time+s_prt_sd+소모시간_뉴스.미디어+시간비율_비즈니스.경제+
                                       횟수비율_비즈니스.경제+횟수_교육.학원+시간비율_커뮤니티+횟수비율_엔터테인먼트+시간비율_서비스,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group55

set.seed(1)
model_rf_cls_group50 <- caret::train(GROUP~
                                     p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                     소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                     시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                     mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                     v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21+시간비율_인터넷.컴퓨터+cat_coef_time+s_prt_sd+소모시간_뉴스.미디어+시간비율_비즈니스.경제,
                               data = cs_merge_train,
                               method = "rf",
                               trControl = fitControl)
model_rf_cls_group50

set.seed(1)
model_rf_cls_group45 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max+
                                       v_pr_sd+횟수비율_온라인교육+s_prt_max+p_v_.13+p_v_.21,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group45

set.seed(1)
model_rf_cls_group40 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit+횟수비율_게임+시간비율_생활.가정.취미+소모시간_온라인교육+p_v_.3+v_t_max,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group40

set.seed(1)
model_rf_cls_group35 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max+
                                       mon_coef_time+횟수비율_인터넷.컴퓨터+시간비율_온라인교육+p_v_.19+cat_coef_visit,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group35

set.seed(1)
model_rf_cls_group30 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4+횟수비율_생활.가정.취미+시간비율_금융.부동산+p_v_.17+소모시간_제조+v_pr_max,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group30

set.seed(1)
model_rf_cls_group25 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조+
                                       시간비율_제조+interval+시간비율_쇼핑+횟수비율_건강.의학+n_v_.4,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group25

set.seed(1)
model_rf_cls_group20 <- caret::train(GROUP~
                                       p_v_.5+횟수비율_쇼핑+p_v_.4+시간비율_뉴스.미디어+횟수_쇼핑+p_v_.11+p_v_.20+v_t_median+mean_time_cnt+s_prt_mean+
                                       소모시간_게임+s_prt_median+n_v_.11+p_v_.9+횟수비율_뉴스.미디어+시간비율_게임+횟수비율_금융.부동산+p_v_.12+p_v_.1+횟수비율_제조,
                                     data = cs_merge_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_cls_group20

########################

#im_rf_clss2v_group
########################
set.seed(1)
model_rf_clss2v_group90 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282+V251+V55+V185+V34+V93+
                                          V259+V199+V183+V280+V182+V149+V266+V42+V92+V257+
                                          V213+V231+V16+V40+V179+V264+V180+V255+V254+V281+
                                          V51+V234+V295+V52+V222+V116+V61+V200+V202+V56+
                                          V124+V289+V21+V19+V113+V155+V196+V37+V170+V35,
                                        data = cs_merge_s2v_train, 
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group90

set.seed(1)
model_rf_clss2v_group85 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282+V251+V55+V185+V34+V93+
                                          V259+V199+V183+V280+V182+V149+V266+V42+V92+V257+
                                          V213+V231+V16+V40+V179+V264+V180+V255+V254+V281+
                                          V51+V234+V295+V52+V222+V116+V61+V200+V202+V56+
                                          V124+V289+V21+V19+V113,
                                        data = cs_merge_s2v_train, 
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group85

set.seed(1)
model_rf_clss2v_group80 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282+V251+V55+V185+V34+V93+
                                          V259+V199+V183+V280+V182+V149+V266+V42+V92+V257+
                                          V213+V231+V16+V40+V179+V264+V180+V255+V254+V281+
                                          V51+V234+V295+V52+V222+V116+V61+V200+V202+V56,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group80

set.seed(1)
model_rf_clss2v_group75 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282+V251+V55+V185+V34+V93+
                                          V259+V199+V183+V280+V182+V149+V266+V42+V92+V257+
                                          V213+V231+V16+V40+V179+V264+V180+V255+V254+V281+
                                          V51+V234+V295+V52+V222,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group75

set.seed(1)
model_rf_clss2v_group70 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282+V251+V55+V185+V34+V93+
                                          V259+V199+V183+V280+V182+V149+V266+V42+V92+V257+
                                          V213+V231+V16+V40+V179+V264+V180+V255+V254+V281,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group70

set.seed(1)
model_rf_clss2v_group65 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282+V251+V55+V185+V34+V93+
                                          V259+V199+V183+V280+V182+V149+V266+V42+V92+V257+
                                          V213+V231+V16+V40+V179,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group65


set.seed(1)
model_rf_clss2v_group60 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282+V251+V55+V185+V34+V93+
                                          V259+V199+V183+V280+V182+V149+V266+V42+V92+V257,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group60

set.seed(1)
model_rf_clss2v_group55 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282+V251+V55+V185+V34+V93+
                                          V259+V199+V183+V280+V182,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group55

set.seed(1)
model_rf_clss2v_group50 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282+V251+V55+V185+V34+V93,
                                     data = cs_merge_s2v_train,
                                     method = "rf",
                                     trControl = fitControl)
model_rf_clss2v_group50

set.seed(1)
model_rf_clss2v_group45 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184+
                                          V7+V57+V233+V31+V282,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group45

set.seed(1)
model_rf_clss2v_group40 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238+V23+V53+V71+V288+V184,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group40

set.seed(1)
model_rf_clss2v_group35 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256+
                                          V215+V154+V207+V130+V238,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group35

set.seed(1)
model_rf_clss2v_group30 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186+V59+V273+V249+V246+V256,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group30

set.seed(1)
model_rf_clss2v_group25 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263+
                                          V294+V28+V274+V106+V186,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group25

set.seed(1)
model_rf_clss2v_group20 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V1+V209+V80+V243+
                                          V160+V125+V175+V86+V77+p_v_.5+V62+p_v_.4+V43+V263,
                                        data = cs_merge_s2v_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_clss2v_group20
########################

#im_rf_s2v300_group
########################
set.seed(1)
model_rf_s2v300_group90 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233+V130+V23+V116+V31+V150+
                                          V184+V288+V231+V202+V255+V75+V40+V51+V93+V92+
                                          V213+V155+V259+V266+V170+V200+V180+V251+V113+V257+
                                          V61+V174+V254+V124+V88+V271+V229+V42+V234+V56+
                                          V149+V287+V264+V12+V37+V147+V182+V281+V89+V245,
                                        data = cs_s2v300_train, 
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group90

set.seed(1)
model_rf_s2v300_group85 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233+V130+V23+V116+V31+V150+
                                          V184+V288+V231+V202+V255+V75+V40+V51+V93+V92+
                                          V213+V155+V259+V266+V170+V200+V180+V251+V113+V257+
                                          V61+V174+V254+V124+V88+V271+V229+V42+V234+V56+
                                          V149+V287+V264+V12+V37,
                                        data = cs_s2v300_train, 
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group85

set.seed(1)
model_rf_s2v300_group80 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233+V130+V23+V116+V31+V150+
                                          V184+V288+V231+V202+V255+V75+V40+V51+V93+V92+
                                          V213+V155+V259+V266+V170+V200+V180+V251+V113+V257+
                                          V61+V174+V254+V124+V88+V271+V229+V42+V234+V56,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group80

set.seed(1)
model_rf_s2v300_group75 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233+V130+V23+V116+V31+V150+
                                          V184+V288+V231+V202+V255+V75+V40+V51+V93+V92+
                                          V213+V155+V259+V266+V170+V200+V180+V251+V113+V257+
                                          V61+V174+V254+V124+V88,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group75

set.seed(1)
model_rf_s2v300_group70 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233+V130+V23+V116+V31+V150+
                                          V184+V288+V231+V202+V255+V75+V40+V51+V93+V92+
                                          V213+V155+V259+V266+V170+V200+V180+V251+V113+V257,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group70

set.seed(1)
model_rf_s2v300_group65 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233+V130+V23+V116+V31+V150+
                                          V184+V288+V231+V202+V255+V75+V40+V51+V93+V92+
                                          V213+V155+V259+V266+V170,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group65

set.seed(1)
model_rf_s2v300_group60 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233+V130+V23+V116+V31+V150+
                                          V184+V288+V231+V202+V255+V75+V40+V51+V93+V92,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group60

set.seed(1)
model_rf_s2v300_group55 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233+V130+V23+V116+V31+V150+
                                          V184+V288+V231+V202+V255,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group55

set.seed(1)
model_rf_s2v300_group50 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233+V130+V23+V116+V31+V150,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group50

set.seed(1)
model_rf_s2v300_group45 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57+
                                          V183+V7+V282+V187+V233,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group45

set.seed(1)
model_rf_s2v300_group40 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199+V71+V207+V185+V53+V57,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group40

set.seed(1)
model_rf_s2v300_group35 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238+
                                          V215+V55+V16+V256+V199,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group35

set.seed(1)
model_rf_s2v300_group30 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249+V154+V59+V246+V34+V238,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group30

set.seed(1)
model_rf_s2v300_group25 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274+
                                          V43+V273+V186+V106+V249,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group25

set.seed(1)
model_rf_s2v300_group20 <- caret::train(GROUP~
                                          V242+V139+V63+V208+V267+V253+V209+V160+V1+V80+
                                          V243+V125+V77+V86+V28+V175+V62+V263+V294+V274,
                                        data = cs_s2v300_train,
                                        method = "rf",
                                        trControl = fitControl)
model_rf_s2v300_group20
########################

#im_xgb_cls_group
########################
set.seed(1)
model_xgb_cls_group90 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학+시간비율_여행+n_v_.16+v_t_median+sep_ratio+cons_cnt18_we+
                                        mean_time+mon_ratio3+wk_ratio2+소모시간_스포츠.레저+횟수비율_교육.학원+v_t_max+day_coef_cnt+횟수비율_비즈니스.경제+nov_ratio2+jan_z2+
                                        시간비율_커뮤니티+p_v_.8+mon_day+fri_ratio3+cat_coef_cnt+wednes_ratio2+소모시간_뉴스.미디어+시간비율_사회.문화.종교+cons_num22+시간비율_서비스+
                                        cons_cnt5+p_v_.21+횟수_비즈니스.경제+mon_z+소모시간_정치.행정+day_coef_visit+cons_time0_ratio_we+cons_num13_we+시간비율_생활.가정.취미+p_v_.17+
                                        횟수_커뮤니티+시간비율_인터넷.컴퓨터+시간비율_비즈니스.경제+dec_ratio2+소모시간_문학.예술+cons_num0_ratio_wk+jan_cnt+cons_time22_wk+cons_cnt8_ratio+cons_time13_we,
                                      data = cs_merge_train, 
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group90[[4]][8])

set.seed(1)
model_xgb_cls_group85 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학+시간비율_여행+n_v_.16+v_t_median+sep_ratio+cons_cnt18_we+
                                        mean_time+mon_ratio3+wk_ratio2+소모시간_스포츠.레저+횟수비율_교육.학원+v_t_max+day_coef_cnt+횟수비율_비즈니스.경제+nov_ratio2+jan_z2+
                                        시간비율_커뮤니티+p_v_.8+mon_day+fri_ratio3+cat_coef_cnt+wednes_ratio2+소모시간_뉴스.미디어+시간비율_사회.문화.종교+cons_num22+시간비율_서비스+
                                        cons_cnt5+p_v_.21+횟수_비즈니스.경제+mon_z+소모시간_정치.행정+day_coef_visit+cons_time0_ratio_we+cons_num13_we+시간비율_생활.가정.취미+p_v_.17+
                                        횟수_커뮤니티+시간비율_인터넷.컴퓨터+시간비율_비즈니스.경제+dec_ratio2+소모시간_문학.예술,
                                      data = cs_merge_train, 
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group85[[4]][8])

set.seed(1)
model_xgb_cls_group80 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학+시간비율_여행+n_v_.16+v_t_median+sep_ratio+cons_cnt18_we+
                                        mean_time+mon_ratio3+wk_ratio2+소모시간_스포츠.레저+횟수비율_교육.학원+v_t_max+day_coef_cnt+횟수비율_비즈니스.경제+nov_ratio2+jan_z2+
                                        시간비율_커뮤니티+p_v_.8+mon_day+fri_ratio3+cat_coef_cnt+wednes_ratio2+소모시간_뉴스.미디어+시간비율_사회.문화.종교+cons_num22+시간비율_서비스+
                                        cons_cnt5+p_v_.21+횟수_비즈니스.경제+mon_z+소모시간_정치.행정+day_coef_visit+cons_time0_ratio_we+cons_num13_we+시간비율_생활.가정.취미+p_v_.17,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group80[[4]][8])

set.seed(1)
model_xgb_cls_group75 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학+시간비율_여행+n_v_.16+v_t_median+sep_ratio+cons_cnt18_we+
                                        mean_time+mon_ratio3+wk_ratio2+소모시간_스포츠.레저+횟수비율_교육.학원+v_t_max+day_coef_cnt+횟수비율_비즈니스.경제+nov_ratio2+jan_z2+
                                        시간비율_커뮤니티+p_v_.8+mon_day+fri_ratio3+cat_coef_cnt+wednes_ratio2+소모시간_뉴스.미디어+시간비율_사회.문화.종교+cons_num22+시간비율_서비스+
                                        cons_cnt5+p_v_.21+횟수_비즈니스.경제+mon_z+소모시간_정치.행정,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group75[[4]][8])

set.seed(1)
model_xgb_cls_group70 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학+시간비율_여행+n_v_.16+v_t_median+sep_ratio+cons_cnt18_we+
                                        mean_time+mon_ratio3+wk_ratio2+소모시간_스포츠.레저+횟수비율_교육.학원+v_t_max+day_coef_cnt+횟수비율_비즈니스.경제+nov_ratio2+jan_z2+
                                        시간비율_커뮤니티+p_v_.8+mon_day+fri_ratio3+cat_coef_cnt+wednes_ratio2+소모시간_뉴스.미디어+시간비율_사회.문화.종교+cons_num22+시간비율_서비스,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group70[[4]][8])

set.seed(1)
model_xgb_cls_group65 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학+시간비율_여행+n_v_.16+v_t_median+sep_ratio+cons_cnt18_we+
                                        mean_time+mon_ratio3+wk_ratio2+소모시간_스포츠.레저+횟수비율_교육.학원+v_t_max+day_coef_cnt+횟수비율_비즈니스.경제+nov_ratio2+jan_z2+
                                        시간비율_커뮤니티+p_v_.8+mon_day+fri_ratio3+cat_coef_cnt,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group65[[4]][8])

set.seed(1)
model_xgb_cls_group60 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학+시간비율_여행+n_v_.16+v_t_median+sep_ratio+cons_cnt18_we+
                                        mean_time+mon_ratio3+wk_ratio2+소모시간_스포츠.레저+횟수비율_교육.학원+v_t_max+day_coef_cnt+횟수비율_비즈니스.경제+nov_ratio2+jan_z2,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group60[[4]][8])

set.seed(1)
model_xgb_cls_group55 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학+시간비율_여행+n_v_.16+v_t_median+sep_ratio+cons_cnt18_we+
                                        mean_time+mon_ratio3+wk_ratio2+소모시간_스포츠.레저+횟수비율_교육.학원,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group55[[4]][8])

set.seed(1)
model_xgb_cls_group50 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학+시간비율_여행+n_v_.16+v_t_median+sep_ratio+cons_cnt18_we,
                                     data = cs_merge_train,
                                     method = "xgbTree",
                                     trControl = fitControl)
min(model_xgb_cls_group50[[4]][8])

set.seed(1)
model_xgb_cls_group45 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max+
                                        횟수비율_서비스+n_v_.4+s_v_mean+cat_coef_visit+횟수비율_건강.의학,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group45[[4]][8])

set.seed(1)
model_xgb_cls_group40 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티+횟수_금융.부동산+s_pr_sd+횟수_교육.학원+s_prt_median+v_pr_max,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group40[[4]][8])

set.seed(1)
model_xgb_cls_group35 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조+
                                        n_v_.2+mean_time_cnt+소모시간_게임+s_prt_sd+횟수비율_커뮤니티,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group35[[4]][8])

set.seed(1)
model_xgb_cls_group30 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20+소모시간_생활.가정.취미+횟수비율_사회.문화.종교+횟수비율_온라인교육+시간비율_금융.부동산+시간비율_제조,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group30[[4]][8])

set.seed(1)
model_xgb_cls_group25 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어+
                                        횟수비율_게임+cons_num9_we+interval+시간비율_온라인교육+p_v_.20,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group25[[4]][8])

set.seed(1)
model_xgb_cls_group20 <- caret::train(GROUP~
                                        p_v_.4+횟수비율_쇼핑+p_v_.5+시간비율_뉴스.미디어+p_v_.12+횟수_쇼핑+횟수_제조+p_v_.19+시간비율_게임+n_v_.11+
                                        p_v_.11+cons_time8_we+소모시간_쇼핑+p_v_.1+s_prt_mean+횟수_생활.가정.취미+p_v_.9+횟수비율_스포츠.레저+p_v_.3+횟수비율_뉴스.미디어,
                                      data = cs_merge_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_cls_group20[[4]][8])
########################

#im_xgb_clss2v_group
########################
set.seed(1)
model_xgb_clss2v_group90 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4+V142+V256+V233+V151+interval+
                                           V140+V19+V100+V230+V182+V183+V263+wk_ratio2+V28+V53+
                                           V126+V254+소모시간_게임+V83+V130+s_v_mean+V143+V7+V222+V216+
                                           V203+V285+V189+V273+p_v_.11+V199+V217+V287+V258+V224+
                                           V89+V58+V102+V264+V124+v_t_median+V3+V300+cons_num13_ratio_wk+mon_ratio2,
                                         data = cs_merge_s2v_train, 
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group90[[4]][8])

set.seed(1)
model_xgb_clss2v_group85 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4+V142+V256+V233+V151+interval+
                                           V140+V19+V100+V230+V182+V183+V263+wk_ratio2+V28+V53+
                                           V126+V254+소모시간_게임+V83+V130+s_v_mean+V143+V7+V222+V216+
                                           V203+V285+V189+V273+p_v_.11+V199+V217+V287+V258+V224+
                                           V89+V58+V102+V264+V124,
                                         data = cs_merge_s2v_train, 
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group85[[4]][8])

set.seed(1)
model_xgb_clss2v_group80 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4+V142+V256+V233+V151+interval+
                                           V140+V19+V100+V230+V182+V183+V263+wk_ratio2+V28+V53+
                                           V126+V254+소모시간_게임+V83+V130+s_v_mean+V143+V7+V222+V216+
                                           V203+V285+V189+V273+p_v_.11+V199+V217+V287+V258+V224,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group80[[4]][8])

set.seed(1)
model_xgb_clss2v_group75 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4+V142+V256+V233+V151+interval+
                                           V140+V19+V100+V230+V182+V183+V263+wk_ratio2+V28+V53+
                                           V126+V254+소모시간_게임+V83+V130+s_v_mean+V143+V7+V222+V216+
                                           V203+V285+V189+V273+p_v_.11,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group75[[4]][8])

set.seed(1)
model_xgb_clss2v_group70 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4+V142+V256+V233+V151+interval+
                                           V140+V19+V100+V230+V182+V183+V263+wk_ratio2+V28+V53+
                                           V126+V254+소모시간_게임+V83+V130+s_v_mean+V143+V7+V222+V216,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group70[[4]][8])

set.seed(1)
model_xgb_clss2v_group65 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4+V142+V256+V233+V151+interval+
                                           V140+V19+V100+V230+V182+V183+V263+wk_ratio2+V28+V53+
                                           V126+V254+소모시간_게임+V83+V130,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group65[[4]][8])

set.seed(1)
model_xgb_clss2v_group60 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4+V142+V256+V233+V151+interval+
                                           V140+V19+V100+V230+V182+V183+V263+wk_ratio2+V28+V53,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group60[[4]][8])

set.seed(1)
model_xgb_clss2v_group55 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4+V142+V256+V233+V151+interval+
                                           V140+V19+V100+V230+V182,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group55[[4]][8])

set.seed(1)
model_xgb_clss2v_group50 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4+V142+V256+V233+V151+interval,
                                      data = cs_merge_s2v_train,
                                      method = "xgbTree",
                                      trControl = fitControl)
min(model_xgb_clss2v_group50[[4]][8])

set.seed(1)
model_xgb_clss2v_group45 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73+
                                           V93+V176+V16+V88+p_v_.4,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group45[[4]][8])

set.seed(1)
model_xgb_clss2v_group40 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20+V239+V149+V57+V113+V73,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group40[[4]][8])

set.seed(1)
model_xgb_clss2v_group35 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180+
                                           V257+V253+V229+V43+p_v_.20,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group35[[4]][8])

set.seed(1)
model_xgb_clss2v_group30 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286+V51+V75+V251+V209+V180,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group30[[4]][8])

set.seed(1)
model_xgb_clss2v_group25 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106+
                                           V154+V135+V56+V86+V286,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group25[[4]][8])

set.seed(1)
model_xgb_clss2v_group20 <- caret::train(GROUP~
                                           V139+V242+V63+V80+V125+V267+V208+p_v_.5+V1+V150+
                                           V243+V207+V77+V175+V62+V294+V55+V249+V71+V106,
                                         data = cs_merge_s2v_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_clss2v_group20[[4]][8])
########################

#im_xgb_s2v300_group
########################
set.seed(1)
model_xgb_s2v300_group90 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286+V116+V102+V158+V57+V253+
                                           V88+V199+V200+V113+V281+V42+V213+V287+V234+V204+
                                           V160+V16+V250+V7+V15+V24+V257+V266+V245+V126+
                                           V288+V251+V189+V37+V146+V216+V276+V93+V233+V100+
                                           V50+V133+V246+V282+V155+V83+V278+V256+V51+V255,
                                         data = cs_s2v300_train, 
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group90[[4]][8])

set.seed(1)
model_xgb_s2v300_group85 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286+V116+V102+V158+V57+V253+
                                           V88+V199+V200+V113+V281+V42+V213+V287+V234+V204+
                                           V160+V16+V250+V7+V15+V24+V257+V266+V245+V126+
                                           V288+V251+V189+V37+V146+V216+V276+V93+V233+V100+
                                           V50+V133+V246+V282+V155,
                                         data = cs_s2v300_train, 
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group85[[4]][8])

set.seed(1)
model_xgb_s2v300_group80 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286+V116+V102+V158+V57+V253+
                                           V88+V199+V200+V113+V281+V42+V213+V287+V234+V204+
                                           V160+V16+V250+V7+V15+V24+V257+V266+V245+V126+
                                           V288+V251+V189+V37+V146+V216+V276+V93+V233+V100,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group80[[4]][8])

set.seed(1)
model_xgb_s2v300_group75 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286+V116+V102+V158+V57+V253+
                                           V88+V199+V200+V113+V281+V42+V213+V287+V234+V204+
                                           V160+V16+V250+V7+V15+V24+V257+V266+V245+V126+
                                           V288+V251+V189+V37+V146,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group75[[4]][8])

set.seed(1)
model_xgb_s2v300_group70 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286+V116+V102+V158+V57+V253+
                                           V88+V199+V200+V113+V281+V42+V213+V287+V234+V204+
                                           V160+V16+V250+V7+V15+V24+V257+V266+V245+V126,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group70[[4]][8])

set.seed(1)
model_xgb_s2v300_group65 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286+V116+V102+V158+V57+V253+
                                           V88+V199+V200+V113+V281+V42+V213+V287+V234+V204+
                                           V160+V16+V250+V7+V15,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group65[[4]][8])

set.seed(1)
model_xgb_s2v300_group60 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286+V116+V102+V158+V57+V253+
                                           V88+V199+V200+V113+V281+V42+V213+V287+V234+V204,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group60[[4]][8])

set.seed(1)
model_xgb_s2v300_group55 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286+V116+V102+V158+V57+V253+
                                           V88+V199+V200+V113+V281,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group55[[4]][8])

set.seed(1)
model_xgb_s2v300_group50 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286+V116+V102+V158+V57+V253,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group50[[4]][8])

set.seed(1)
model_xgb_s2v300_group45 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53+
                                           V186+V183+V8+V149+V286,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group45[[4]][8])

set.seed(1)
model_xgb_s2v300_group40 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140+V207+V3+V73+V43+V53,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group40[[4]][8])

set.seed(1)
model_xgb_s2v300_group35 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285+
                                           V264+V249+V19+V130+V140,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group35[[4]][8])

set.seed(1)
model_xgb_s2v300_group30 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56+V243+V142+V222+V143+V285,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group30[[4]][8])

set.seed(1)
model_xgb_s2v300_group25 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106+
                                           V75+V229+V182+V154+V56,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group25[[4]][8])

set.seed(1)
model_xgb_s2v300_group20 <- caret::train(GROUP~
                                           V139+V242+V80+V63+V125+V71+V208+V267+V1+V175+
                                           V55+V62+V135+V294+V77+V86+V28+V209+V150+V106,
                                         data = cs_s2v300_train,
                                         method = "xgbTree",
                                         trControl = fitControl)
min(model_xgb_s2v300_group20[[4]][8])
########################


models<-resamples(list(model_rf_cls_group=model_rf_cls_group50,
                       model_rf_clss2v_group=model_rf_clss2v_group50,
                       model_rf_s2v300_group=model_rf_s2v300_group50,
                       model_xgb_cls_group=model_xgb_cls_group50,
                       model_xgb_clss2v_group=model_xgb_clss2v_group50,
                       model_xgb_s2v300_group=model_xgb_s2v300_group50))
summary(models)
dotplot(models)
#correlation table (possible for more than two models)
cor_models=modelCor(models)

#Methods for making inferences about differences between models: t-test(p-value) (possible for only two models)
compare_models(model_rf_cls_group50,
               model_rf_clss2v_group50)
