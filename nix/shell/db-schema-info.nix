{
  lib,
  postgresql,
  coreutils,
  schemacrawler,
  writeText,
  writeShellApplication,
}:
writeShellApplication {
  name = "db-schema-info";
  runtimeInputs = [coreutils postgresql schemacrawler];
  text = ''
    set -euo pipefail

    DB_HOST="''${DB_HOST:-localhost}"
    DB_PORT="''${DB_PORT:-15432}"
    DB_USER="''${DB_USER:-marlowe}"
    DB_PASSWORD="''${DB_PASSWORD:-marlowe}"
    DB_NAME="''${DB_NAME:-marlowe}"
    SCHEMA="''${SCHEMA:-marlowe}"
    LAYOUT="''${LAYOUT:-TB}"

    OUTPUT_DIR="''${1:-./sql/final}"
    mkdir -p "$OUTPUT_DIR"

    JDBC_URL="jdbc:postgresql://$DB_HOST:$DB_PORT/$DB_NAME"

    DOT_OUTPUT="$OUTPUT_DIR/marlowe-database.dot"

    schemacrawler \
        --url="$JDBC_URL" \
        --user="$DB_USER" \
        --password="$DB_PASSWORD" \
        --command schema \
        --info-level=maximum \
        --output-format scdot \
        --output-file="$DOT_OUTPUT" \
        --schemas="$SCHEMA" \
        2>&1

    if [ ! -f "$DOT_OUTPUT" ]; then
        echo "Error: SchemaCrawler failed to generate DOT file"
        exit 1
    fi

    sed -i "s/rankdir=\"[A-Z][A-Z]\"/rankdir=\"$LAYOUT\"/" "$DOT_OUTPUT"

    SVG_OUTPUT="$OUTPUT_DIR/marlowe-database.svg"
    dot -Tsvg "$DOT_OUTPUT" -o "$SVG_OUTPUT" 2>/dev/null

    if [ ! -f "$SVG_OUTPUT" ]; then
        echo "Error: Graphviz failed to generate SVG"
        exit 1
    fi

    SQL_OUTPUT="$OUTPUT_DIR/marlowe-database.sql"
    pg_dump -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" \
        --schema-only \
        --schema="$SCHEMA" \
        > "$SQL_OUTPUT" 2>/dev/null

    if [ ! -f "$SQL_OUTPUT" ]; then
        echo "Error: pg_dump failed to generate SQL"
        exit 1
    fi

    echo "Generated:"
    echo "  $DOT_OUTPUT"
    echo "  $SVG_OUTPUT"
    echo "  $SQL_OUTPUT"
  '';
}
