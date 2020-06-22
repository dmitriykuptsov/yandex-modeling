# Прогнозирование запросов к поисковой системе Yandex

Очевидно, что во временном ряду есть как влияние времени года (сезонность), так и тренд (количество запросов
растет в целом). Причём графики для Yandex и для Google похожи (у Yandex количество запросов растет немного 
быстрее - коэффициент линейной модели больше, чем у Google). Как видно из графика, количество запросов резко 
падает во время зимних и весенних (летних) каникул в России. Но с другой стороны, в целом, количество 
запросов растет со временем. Исходя из этого, скорее всего, для прогнозирования подойдёт метод 
Хольта-Винтерса, который учитывает сезонность и тренд.

Для анализа мы будем применять следующие методы:

(i) Линейная регрессия

(ii) Метод Хольта-Винтерса, который используется для прогнозирования временных рядов, когда в структуре 
данных есть сложившийся тренд и сезонность.

Исходя из прогноза, можно сделать вывод, что примерно в декабре 2020/январе 2021 года количество запросов 
достигнет минимума (количество запросов начнёт снижаться где-то в октябре/ноябре). Та же картина 
просматривается и для конца весны/середины лета (где-то в конце мая/начале июня количество запросов начнёт снижаться и минимум придётся на июнь/июль). Риском можно считать чрезвычайные ситуации, (например, политические волнения или стихийные бедствия, когда люди будут массово искать что-то в сети Интернет (т.е. эти события не являются трендом, а также не зависят от времени года). Тогда картина может измениться и прогноз не будет достоверным.

# Литература

Forecasting: Principles and Practice, Rob J Hyndman and George Athanasopoulos, Online: https://otexts.com/fpp2/
