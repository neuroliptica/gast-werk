# gast-werk
Простой клиет-сервер для совместного создания датасетов или решения капч. 

Упор сделан на правильное распределение данных (одна капча одному клиенту, синхронизация решённых капч, обработка aborted капч и т.д.), при этом ничего более эта штука не делает. 

![arch](https://imgur.com/5xmsKdB.jpg)

## Собрать
###### Go >= 1.18
```bash
$ git clone https://github.com/neuroliptica/gast-werk.git
$ cd gast-werk
$ go build
```

## TODO
- Пофиксить дата-рейсы get-solve рутин.
- Чекер для Queue таблицы.
- Админка для простого и удобного деплоя.
- Возможно немного фронта.
- Расписать доку.
