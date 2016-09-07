#ifndef UTILS_H
#define UTILS_H

#include <hunspell.hxx>
#include <iconv.h>
#include <R_ext/Riconv.h>
#include <errno.h>
#include <Rcpp.h>

class hunspell_dict {
  Hunspell * pMS_;
  iconv_t cd_from_;
  iconv_t cd_to_;
  std::string enc_;

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
  hunspell_dict(Rcpp::String affix, Rcpp::CharacterVector dicts){
    std::string dict(dicts[0]);
    pMS_ = new Hunspell(affix.get_cstring(), dict.c_str());
    if(!pMS_)
      throw std::runtime_error(std::string("Failed to load file ") + dict);

    //add additional dictionaries if more than one
    //assuming the same affix?? This can cause unpredictable behavior
    for(int i = 1; i < dicts.length(); i++)
      pMS_->add_dic(std::string(dicts[0]).c_str());

    enc_ = pMS_->get_dict_encoding();
    cd_from_ = new_iconv("UTF-8", enc_.c_str());
    cd_to_ = new_iconv(enc_.c_str(), "UTF-8");
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
    for (const std::basic_string<char>& x : pMS_->suggest(str)) {
      out.push_back(string_to_r(x.c_str()));
    }
    free(str);
    return out;
  }

  Rcpp::CharacterVector analyze(Rcpp::String word){
    Rcpp::CharacterVector out;
    char * str = string_from_r(word);
    for (const std::basic_string<char>& x : pMS_->analyze(str)) {
      out.push_back(string_to_r(x.c_str()));
    }
    free(str);
    return out;
  }

  Rcpp::CharacterVector stem(Rcpp::String word){
    Rcpp::CharacterVector out;
    char * str = string_from_r(word);
    for (const std::basic_string<char>& x : pMS_->stem(str)) {
      out.push_back(string_to_r(x.c_str()));
    }
    free(str);
    return out;
  }

  //adds ignore words to the dictionary
  void add_words(Rcpp::StringVector words){
    for(int i = 0; i < words.length(); i++){
      add_word(words[i]);
    }
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

  Rcpp::RawVector r_wordchars(){
    const char * charvec = NULL;
    size_t rawlen = 0;
    if(is_utf8()){
      const std::vector<w_char>& vec_wordchars_utf16 = pMS_->get_wordchars_utf16();
      rawlen = vec_wordchars_utf16.size() * 2;
      charvec = rawlen ? (const char *) &vec_wordchars_utf16[0] : NULL;
    } else {
      charvec = pMS_->get_wordchars().c_str();
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

#endif // UTILS_H
