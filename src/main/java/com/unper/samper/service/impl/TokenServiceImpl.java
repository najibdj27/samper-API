package com.unper.samper.service.impl;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.time.DateUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Token;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.EType;
import com.unper.samper.repository.TokenRepository;
import com.unper.samper.service.TokenService;

@Service
public class TokenServiceImpl implements TokenService {

    @Autowired
    TokenRepository tokenRepository;

    @Value("${com.unper.samper.token-expiration-ms}")
    int tokenExpiration;

    @Override
    public Token create(Token token) throws ResourceAlreadyExistException, ResourceNotFoundException {
        if (tokenRepository.findByKey(token.getKey()).isPresent()) {
            deleteByKey(token.getKey());
        }
        Date now = Calendar.getInstance().getTime();
        token.setExpiredDate(DateUtils.addMilliseconds(now, tokenExpiration));
        Token newToken = tokenRepository.save(token);
        return newToken;
    }

    @Override
    public Token getById(Long id) throws ResourceNotFoundException {
        return tokenRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("Token with id "+id+" not found!"));
    }
    
    @Override
    public Token getByKey(String key) throws ResourceNotFoundException {
        return tokenRepository.findByKey(key).orElseThrow(() -> new ResourceNotFoundException("Token with key '"+key+"' not found!"));
    }
    
    @Override
    public Token getByKeyAndType(String key, EType type) throws ResourceNotFoundException {
        return tokenRepository.findByKeyAndType(key, type).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.TOKEN_NOT_EXISST.getMessage()));
    }

    @Override
    public void deleteByKey(String key) throws ResourceNotFoundException {
        getByKey(key);
        tokenRepository.deleteBykey(key);
    }

    @Override
    @Scheduled(cron = "0 * * * * *")
    public void houskeeper() {
        LocalDateTime localDateTime = LocalDateTime.now();
        Date expiredDate = Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());
        List<Token> tokenList = tokenRepository.findByLessThanExpiredDate(expiredDate);
        tokenRepository.deleteAll(tokenList);
    }

    @Override
    public boolean isExpired(Long id, Date date) throws ResourceNotFoundException {
        Boolean isExpired = tokenRepository.isExpired(id, date);
        return isExpired;
    }
}
