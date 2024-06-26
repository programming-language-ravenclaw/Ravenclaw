module Statement.CommentParser
  ( comment,
    lineComment,
    blockComment,
  )
where

import AST.AST
import Text.Parsec
import Text.Parsec.Text (Parser)

-- | 'comment' tries to parse either a line comment or a block comment.
-- It first tries to parse a line comment, and if that fails, it tries to parse a block comment.
-- Example:
-- 
-- >>> parse comment "" "# This is a line comment\n"
-- Right (LineComment " This is a line comment")
--
-- >>> parse comment "" "## This is a block comment ##"
-- Right (BlockComment " This is a block comment ")
comment :: Parser Comment
comment = try lineComment <|> try blockComment

-- | 'lineComment' parses a single line comment.
-- A line comment starts with a single '#' and goes until the end of the line.
-- It does not parse comments that start with '##'.
-- Example: 
-- # Comments for this line
lineComment :: Parser Comment
lineComment = LineComment <$> (try (string "#" <* notFollowedBy (char '#')) *> manyTill anyChar newline)

-- | 'blockComment' parses a block comment.
-- A block comment starts with '##' and ends with '##'.
-- Example:
-- ## Comments
-- for the block ##
blockComment :: Parser Comment
blockComment = BlockComment <$> (string "##" *> manyTill anyChar (try (string "##")))