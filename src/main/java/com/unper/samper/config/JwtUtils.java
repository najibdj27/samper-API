package com.unper.samper.config;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import com.unper.samper.exception.ExpiredTokenException;
import com.unper.samper.model.common.UserDetailsImpl;

import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.SignatureException;
import io.jsonwebtoken.UnsupportedJwtException;

@Component
public class JwtUtils {
    @Value("${com.unper.samper.security.jwt.secret}")
    private String jwtSecret;

    @Value("${com.unper.samper.security.jwt.access-token-expiration-ms}")
    private long accessTokenExpirationMs;
   
    @Value("${com.unper.samper.security.jwt.refresh-token-expiration-ms}")
    private long refreshTokenExpirationMs;

    public Map<String, String> generateJwtToken(Authentication authentication) {
        UserDetailsImpl userPrincipal = (UserDetailsImpl) authentication.getPrincipal();

        String accessToken = Jwts.builder()
            .setSubject((userPrincipal.getUsername()))
            .setIssuedAt(new Date())
            .setExpiration(new Date((new Date()).getTime() + accessTokenExpirationMs))
            .signWith(SignatureAlgorithm.HS512, jwtSecret)
            .compact();
        
        
        String refreshToken = Jwts.builder()
            .setSubject((userPrincipal.getUsername()))
            .setIssuedAt(new Date())
            .setExpiration(new Date((new Date()).getTime() + refreshTokenExpirationMs))
            .signWith(SignatureAlgorithm.HS512, jwtSecret)
            .compact();

        Map<String, String> newToken = new HashMap<>();
        newToken.put("accessToken", accessToken);
        newToken.put("refreshToken", refreshToken);
        
        return newToken;
    }

    public String refreshAccessToken(String refreshToken) throws ExpiredTokenException {
        String username = getUserNameFromJwtToken(refreshToken);
        if (Boolean.TRUE.equals(validateJwtToken(refreshToken))) {
            return Jwts.builder()
            .setSubject(username)
            .setIssuedAt(new Date())
            .setExpiration(new Date((new Date()).getTime() + accessTokenExpirationMs))
            .signWith(SignatureAlgorithm.HS512, jwtSecret)
            .compact();
        } else {
            throw new ExpiredTokenException("Your refresh token is expired, please log in again!");
        }
    }

    public String getUserNameFromJwtToken(String token) {
        return Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(token).getBody().getSubject();
    }

    public boolean validateJwtToken(String authToken) {
        try {
            Jwts.parser().setSigningKey(jwtSecret).parseClaimsJws(authToken);
            return true;
        } catch (SignatureException e) {
            return false;
        } catch (MalformedJwtException e) {
            return false;
        } catch (ExpiredJwtException e) {
            return false;
        } catch (UnsupportedJwtException e) {
            return false;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }
}