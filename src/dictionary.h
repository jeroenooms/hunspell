#include <hunspell.hxx>
#include <iconv.h>
#include <R_ext/Riconv.h>
#include <errno.h>
#include <Rcpp.h>

class LIBHUNSPELL_DLL_EXPORTED hunspell_dict {
  Hunspell * pMS_;
  iconv_t cd_from_;
  iconv_t cd_to_;
  std::string enc_;
  Rcpp::String affix_;
  Rcpp::CharacterVector dicts_;
  Rcpp::StringVector added_;

private:
  iconv_t new_iconv(const char * from, const char * to){
    iconv_t cd = (iconv_t) Riconv_open(to, from);
    if(cd == (iconv_t) -1){
      switch(errno){
        case EINVAL: throw std::runtime_error(std::string("Unsupported iconv conversion: ") + from + "to" + to);
        default: throw std::runtime_error("General error in Riconv_open()");
      }
    }
    return cd;
  }

public:
  // Some strings are regular strings
  hunspell_dict(Rcpp::String affix, Rcpp::CharacterVector dicts, Rcpp::StringVector words) : affix_(affix), dicts_(dicts), added_(words) {
    std::string dict(dicts[0]);
    pMS_ = new Hunspell(affix.get_cstring(), dict.c_str());
    if(!pMS_)
      throw std::runtime_error(std::string("Failed to load file ") + dict);

    //add additional dictionaries if more than one
    //assuming the same affix?? This can cause unpredictable behavior
    for(int i = 1; i < dicts.length(); i++)
      pMS_->add_dic(std::string(dicts[i]).c_str());

    //setup iconv converters
    enc_ = pMS_->get_dict_encoding();
    cd_from_ = new_iconv("UTF-8", enc_.c_str());
    cd_to_ = new_iconv(enc_.c_str(), "UTF-8");

    //add custom words to dictionary
    for(int i = 0; i < words.length(); i++)
      add_word(words.at(i));
  }

  ~hunspell_dict() {
    try {
      Riconv_close(cd_from_);
      Riconv_close(cd_to_);
      delete pMS_;
    } catch (...) {}
  }

  unsigned short * get_wordchars_utf16(int *len){
    return (unsigned short *) pMS_->get_wordchars_utf16().data();
  }

  bool spell(std::string str){
    return pMS_->spell(str);
  }

  bool spell(Rcpp::String word){
    char * str = string_from_r(word);
    // Words that cannot be converted into the required encoding are by definition incorrect
    if(str == NULL)
      return false;
    bool res = pMS_->spell(std::string(str));
    free(str);
    return res;
  }

  void add_word(Rcpp::String word){
    char * str = string_from_r(word);
    if(str != NULL) {
      pMS_->add_with_affix(str, "a"); //Workaround for https://github.com/ropensci/hunspell/issues/29
      pMS_->add(str);
      free(str);
    }
  }

  std::string enc(){
    return enc_;
  }

  bool is_utf8(){
    return (
      !strcmp(enc_.c_str(), "UTF-8") || !strcmp(enc_.c_str(), "utf8") ||
      !strcmp(enc_.c_str(), "UTF8") ||!strcmp(enc_.c_str(), "utf-8")
    );
  }

  Rcpp::CharacterVector suggest(Rcpp::String word){
    char * str = string_from_r(word);
    Rcpp::CharacterVector out;
    std::vector<std::string> x = pMS_->suggest(str);
    for (size_t i = 0; i < x.size(); i++) {
      out.push_back(string_to_r(x.at(i).c_str()));
    }
    free(str);
    return out;
  }

  Rcpp::CharacterVector analyze(Rcpp::String word){
    Rcpp::CharacterVector out;
    char * str = string_from_r(word);
    std::vector<std::string> x = pMS_->analyze(str);
    for (size_t i = 0; i < x.size(); i++) {
      out.push_back(string_to_r(x.at(i).c_str()));
    }
    free(str);
    return out;
  }

  Rcpp::CharacterVector stem(Rcpp::String word){
    Rcpp::CharacterVector out;
    char * str = string_from_r(word);
    if (!str)
      return out;
    std::vector<std::string> x = pMS_->stem(str);
    for (size_t i = 0; i < x.size(); i++) {
      out.push_back(string_to_r(x.at(i).c_str()));
    }
    free(str);
    return out;
  }

  iconv_t cd_from(){
    return cd_from_;
  }

  iconv_t cd_to(){
    return cd_to_;
  }

  std::string wc(){
    return pMS_->get_wordchars();
  }

  Rcpp::String affix(){
    return affix_;
  }

  Rcpp::CharacterVector dicts(){
    return dicts_;
  }

  Rcpp::CharacterVector added(){
    return added_;
  }

  Rcpp::RawVector r_wordchars(){
    const char * charvec = NULL;
    size_t rawlen = 0;
    if(is_utf8()){
      const std::vector<w_char>& vec_wordchars_utf16 = pMS_->get_wordchars_utf16();
      rawlen = vec_wordchars_utf16.size() * 2;
      charvec = rawlen ? (const char *) &vec_wordchars_utf16[0] : NULL;
    } else {
      charvec = pMS_->get_wordchars();
      rawlen = strlen(charvec);
    }
    Rcpp::RawVector out(rawlen);
    if(rawlen > 0)
      memcpy(out.begin(), charvec, rawlen);
    return out;
  }

  std::vector<w_char> get_wordchars_utf16(){
    return pMS_->get_wordchars_utf16();
  }

  char * string_from_r(Rcpp::String str){
    str.set_encoding(CE_UTF8);
    const char * inbuf = str.get_cstring();
    size_t inlen = strlen(inbuf);
    size_t outlen = 4 * inlen + 1;
    char * output = (char *) malloc(outlen);
    char * cur = output;
    size_t success = Riconv(cd_from_, &inbuf, &inlen, &cur, &outlen);
    if(success == (size_t) -1){
      free(output);
      return NULL;
    }
    *cur = '\0';
    output = (char *) realloc(output, outlen + 1);
    return output;
  }

  Rcpp::String string_to_r(const char * inbuf){
    if(inbuf == NULL)
      return NA_STRING;
    size_t inlen = strlen(inbuf);
    size_t outlen = 4 * inlen + 1;
    char * output = (char *) malloc(outlen);
    char * cur = output;
    size_t success = Riconv(cd_to_, &inbuf, &inlen, &cur, &outlen);
    if(success == (size_t) -1){
      free(output);
      return NA_STRING;
    }
    *cur = '\0';
    Rcpp::String res = Rcpp::String(output);
    res.set_encoding(CE_UTF8);
    free(output);
    return res;
  }
};
