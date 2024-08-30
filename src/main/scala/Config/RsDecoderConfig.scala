package Rs

case class RSDecoderConfig(
  forneyErrataLocTermsPerCycle: Int,
  forneyErrEvalTermsPerCycle: Int,
  forneyEEXlInvTermsPerCycle: Int,
  forneyEEXlInvComboLen: Int,
  forneyEvTermsPerCycle: Int,
)

object RSDecoderConfigs {
  val RS255_239 = RSDecoderConfig(
    forneyErrataLocTermsPerCycle = 2,
    forneyErrEvalTermsPerCycle = 2,
    forneyEEXlInvTermsPerCycle = 2,
    forneyEEXlInvComboLen = 2,
    forneyEvTermsPerCycle = 3,
  )

  val RS108_106 = RSDecoderConfig(
    forneyErrataLocTermsPerCycle = 1,
    forneyErrEvalTermsPerCycle = 1,
    forneyEEXlInvTermsPerCycle = 1,
    forneyEEXlInvComboLen = 1,
    forneyEvTermsPerCycle = 1,
  )

  // Method to return the appropriate config based on N_LEN and K_LEN
  def getConfig(N_LEN: Int, K_LEN: Int): RSDecoderConfig = {
    (N_LEN, K_LEN) match {
      case (255, 239) => RS255_239
      case (108, 106) => RS108_106
      case _ => RS255_239
    }
  }
}
