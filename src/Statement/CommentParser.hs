module Statement.CommentParser (
    comment,
    lineComment,
    blockComment
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST

comment :: Parser Comment
comment = try lineComment <|> try blockComment

lineComment :: Parser Comment
lineComment = LineComment <$> (try (string "#" <* notFollowedBy (char '#')) *> manyTill anyChar newline)

blockComment :: Parser Comment
blockComment = BlockComment <$> (string "##" *> manyTill anyChar (try (string "##")))