FROM ubuntu

WORKDIR /app

COPY --chown=1001 . .

RUN apt update \
    && apt install -y \
    python3.10 \
    python3-pip \
    libcairo2 \
    cairosvg

RUN pip install -r ./requirements.txt

USER 1001

ENTRYPOINT [ "python3", "shepherd.py" ]
