# gast-werk
Простой клиет-сервер для совместного создания датасетов или решения капч. 

Упор сделан на правильное распределение данных (одна капча одному клиенту, синхронизация решённых капч, обработка aborted капч и т.д.), при этом ничего более эта штука не делает. 

![arch](https://imgur.com/5xmsKdB.jpg)

## API
- GET `/api/get`

    ```json
    "Response"
    {
        "img_base64": "base64/data...",
        "hash": "...",
        "empty": 0
    }
    ```

- POST `/api/solve`

    ```json
    "Request"
    {
        "hash": "...",
        "value": "..."
    }

    "Response"
    {
        "status": "msg",
        "ok": 1
    }
    ```

## Собрать
###### Go >= 1.18
```bash
$ git clone https://github.com/neuroliptica/gast-werk.git
$ cd gast-werk
$ go build
```

## TODO
- Админка для простого и удобного деплоя.
- Возможно немного фронта.
- Расписать доку.
